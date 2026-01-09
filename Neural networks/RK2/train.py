import os
import random

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import seaborn as sns
import torch
import torch.nn as nn
import torch.optim as optim
from PIL import Image
from sklearn.metrics import f1_score
from sklearn.model_selection import train_test_split
from torch.utils.data import Dataset, DataLoader
from torchvision import transforms, models
from torchvision.models import Wide_ResNet50_2_Weights
from tqdm import tqdm

RANDOM_SEED = 20122125
BATCH_SIZE = 40
NUM_EPOCHS = 45
LEARNING_RATE = 8e-5
IMG_SIZE = 224
NUM_CLASSES = 50
VAL_SPLIT = 0.18

# для регуляризации
WEIGHT_DECAY = 2e-4
DROPOUT = 0.4
LABEL_SMOOTHING = 0.1

# пути к данным
DATA_DIR = 'data'
LABEL_PATH = 'label.csv'
MODEL_SAVE_PATH = 'model.pth'


def perform_eda(df):
    class_counts = df.iloc[:, 1].value_counts().sort_index()

    df_counts = pd.DataFrame({
        "class": class_counts.index,
        "count": class_counts.values
    })

    plt.figure(figsize=(16, 8))
    sns.barplot(data=df_counts, x="class", y="count", palette="viridis")
    plt.xticks(rotation=45)
    plt.title("Распределение изображений по классам", fontsize=16, pad=20)
    plt.xlabel("Класс")
    plt.ylabel("Количество изображений")

    mean_val = df_counts["count"].mean()
    plt.axhline(mean_val, color="red", linestyle="--", linewidth=2)

    plt.text(
        len(df_counts) + 0.5,
        mean_val,
        f"Среднее: {mean_val:.0f}",
        va="center",
        ha="left",
        color="red",
        fontsize=12,
        backgroundcolor="white"
    )

    plt.tight_layout()
    plt.show()

    print("\nСтатистика по классам:")
    print(f"Минимальное количество изображений в классе: {class_counts.min()}")
    print(f"Максимальное количество изображений в классе: {class_counts.max()}")
    print(f"Среднее количество изображений в классе: {class_counts.mean():.1f}")
    print(f"Стандартное отклонение: {class_counts.std():.1f}")
    print()
    return df_counts


# нужно для воспроизводимости
def set_seed(seed):
    random.seed(seed)
    np.random.seed(seed)
    torch.manual_seed(seed)
    if torch.cuda.is_available():
        torch.cuda.manual_seed_all(seed)
        torch.backends.cudnn.benchmark = True


set_seed(RANDOM_SEED)

device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')


class ImageDataset(Dataset):
    def __init__(self, dataframe, img_dir, transform=None):
        self.data = dataframe.reset_index(drop=True)
        self.img_dir = img_dir
        self.transform = transform

    def __len__(self):
        return len(self.data)

    def __getitem__(self, idx):
        img_name = self.data.iloc[idx, 0]
        label = int(self.data.iloc[idx, 1])

        img_path = os.path.join(self.img_dir, img_name)
        image = Image.open(img_path).convert('RGB')

        if self.transform:
            image = self.transform(image)

        return image, label


def get_train_transforms():
    return transforms.Compose([
        # Геометрические трансформации
        transforms.RandomResizedCrop(IMG_SIZE, scale=(0.75, 1.0)),
        transforms.RandomHorizontalFlip(p=0.5),
        transforms.RandomVerticalFlip(p=0.2),
        transforms.RandomRotation(25),

        # Цветовые трансформации
        transforms.ColorJitter(
            brightness=0.3,
            contrast=0.3,
            saturation=0.3,
            hue=0.15
        ),

        # Аффинные трансформации
        transforms.RandomAffine(
            degrees=0,
            translate=(0.15, 0.15)
        ),

        # Перспективные искажения
        transforms.RandomPerspective(
            distortion_scale=0.2,
            p=0.3
        ),

        # Нормализация
        transforms.ToTensor(),
        transforms.Normalize([0.485, 0.456, 0.406], [0.229, 0.224, 0.225]),

        # Случайное стирание областей
        transforms.RandomErasing(
            p=0.3,
            scale=(0.02, 0.15),
            ratio=(0.3, 3.3)
        ),
    ])


def get_val_transforms():
    # трансформации для валидации без аугментаций
    return transforms.Compose([
        transforms.Resize(256),
        transforms.CenterCrop(IMG_SIZE),
        transforms.ToTensor(),
        transforms.Normalize([0.485, 0.456, 0.406], [0.229, 0.224, 0.225])
    ])


def create_model():
    model = models.wide_resnet50_2(weights=Wide_ResNet50_2_Weights.DEFAULT)

    # заменяем последний слой
    in_features = model.fc.in_features
    model.fc = nn.Sequential(
        nn.Dropout(DROPOUT),
        nn.Linear(in_features, NUM_CLASSES)
    )

    return model


def train_epoch(model, dataloader, criterion, optimizer, device):
    # активируем dropout на обучении
    model.train()

    running_loss = 0.0
    all_predictions = []
    all_labels = []

    progress_bar = tqdm(dataloader, desc='Training')

    for images, labels in progress_bar:
        images = images.to(device)
        labels = labels.to(device)
        optimizer.zero_grad()

        # forward
        outputs = model(images)
        loss = criterion(outputs, labels)

        # backward
        loss.backward()
        optimizer.step()

        running_loss += loss.item()
        _, predictions = torch.max(outputs, 1)
        all_predictions.extend(predictions.cpu().numpy())
        all_labels.extend(labels.cpu().numpy())

        progress_bar.set_postfix({'loss': f'{loss.item():.4f}'})

    epoch_loss = running_loss / len(dataloader)
    epoch_f1 = f1_score(all_labels, all_predictions, average='macro')

    return epoch_loss, epoch_f1


def validate(model, dataloader, criterion, device):
    # выключаем dropout
    model.eval()

    running_loss = 0.0
    all_predictions = []
    all_labels = []

    with torch.no_grad():
        for images, labels in tqdm(dataloader, desc='Validation'):
            images = images.to(device)
            labels = labels.to(device)

            # forward
            outputs = model(images)
            loss = criterion(outputs, labels)

            running_loss += loss.item()
            _, predictions = torch.max(outputs, 1)
            all_predictions.extend(predictions.cpu().numpy())
            all_labels.extend(labels.cpu().numpy())

    epoch_loss = running_loss / len(dataloader)
    epoch_f1 = f1_score(all_labels, all_predictions, average='macro')

    return epoch_loss, epoch_f1


def main():
    print(f"Устройство: {device}")
    print(f"RANDOM_SEED: {RANDOM_SEED}")
    print(f"Batch size: {BATCH_SIZE}")
    print(f"Learning rate: {LEARNING_RATE}")
    print(f"Epochs: {NUM_EPOCHS}")
    print(f"Dropout: {DROPOUT}")
    print(f"Weight decay: {WEIGHT_DECAY}")
    print(f"Label smoothing: {LABEL_SMOOTHING}")
    print("-" * 60)

    print("Загрузка данных...")
    df = pd.read_csv(LABEL_PATH)
    print(f"Всего изображений: {len(df)}")
    print(f"Классов: {df.iloc[:, 1].nunique()}")
    perform_eda(df)

    # разделение на train и validation с сохранением пропорции классов
    train_df, val_df = train_test_split(
        df,
        test_size=VAL_SPLIT,
        random_state=RANDOM_SEED,
        stratify=df.iloc[:, 1]
    )

    print(f"Train: {len(train_df)} | Val: {len(val_df)}")
    print("-" * 60)

    train_dataset = ImageDataset(train_df, DATA_DIR, get_train_transforms())
    val_dataset = ImageDataset(val_df, DATA_DIR, get_val_transforms())

    # создание загрузчиков данных
    train_loader = DataLoader(
        train_dataset,
        batch_size=BATCH_SIZE,
        shuffle=True,  # перемешиваем train для предотвращения запоминания порядка данных
        num_workers=2,
        pin_memory=True
    )

    val_loader = DataLoader(
        val_dataset,
        batch_size=BATCH_SIZE,
        shuffle=False,  # не перемешиваем
        num_workers=2,
        pin_memory=True
    )

    print("Создание модели...")
    model = create_model().to(device)

    # добавляем label_smoothing для повышения устойчивости к шуму
    criterion = nn.CrossEntropyLoss(label_smoothing=LABEL_SMOOTHING)

    # AdamW для лучшей регуляризации
    optimizer = optim.AdamW(
        model.parameters(),
        lr=LEARNING_RATE,
        weight_decay=WEIGHT_DECAY
    )

    # помогает выйти из локальных минимумов
    scheduler = optim.lr_scheduler.CosineAnnealingWarmRestarts(
        optimizer,
        T_0=10,
        T_mult=1,
        eta_min=1e-6
    )

    # параметры для ранней остановки
    best_val_f1 = 0.0
    patience_counter = 0
    max_patience = 10

    print("\nНачало обучения...")
    print("=" * 60)

    for epoch in range(NUM_EPOCHS):
        print(f"\nЭпоха {epoch + 1}/{NUM_EPOCHS}")
        print("-" * 60)

        train_loss, train_f1 = train_epoch(model, train_loader, criterion, optimizer, device)
        val_loss, val_f1 = validate(model, val_loader, criterion, device)

        print(f"Train Loss: {train_loss:.4f} | Train F1: {train_f1:.4f}")
        print(f"Val Loss:   {val_loss:.4f} | Val F1:   {val_f1:.4f}")

        # обновляем learning rate
        old_lr = optimizer.param_groups[0]['lr']
        scheduler.step()
        new_lr = optimizer.param_groups[0]['lr']

        if abs(new_lr - old_lr) / old_lr > 0.1:
            print(f"Learning rate: {old_lr:.6f} → {new_lr:.6f}")

        # сохраняем лучшую модель
        if val_f1 > best_val_f1:
            best_val_f1 = val_f1
            torch.save(model.state_dict(), MODEL_SAVE_PATH)
            print(f"✓ Модель сохранена! Лучший Val F1: {best_val_f1:.4f}")
            patience_counter = 0
        else:
            patience_counter += 1
            print(f"Без улучшения: {patience_counter}/{max_patience}")

        # останавливаемся, если Val F1 не растет
        if patience_counter >= max_patience:
            print(f"\nEarly stopping на эпохе {epoch + 1}")
            print("Модель перестала улучшаться, завершаем обучение")
            break

    print("\n" + "=" * 60)
    print(f"Обучение завершено!")
    print(f"Лучший Val F1: {best_val_f1:.4f}")
    print("=" * 60)


if __name__ == '__main__':
    main()
