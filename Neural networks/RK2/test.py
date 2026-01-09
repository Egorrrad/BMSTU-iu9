import torch
import torch.nn as nn
from torchvision import transforms, models
from PIL import Image
import pandas as pd
import os
import sys
from glob import glob
import argparse
from tqdm import tqdm

NUM_CLASSES = 50
BATCH_SIZE = 40
IMG_SIZE = 224
DROPOUT = 0.4


class ImageClassifier:
    def __init__(self, model_path='model.pth', device=None):
        self.num_classes = NUM_CLASSES
        self.device = device if device else torch.device('cuda' if torch.cuda.is_available() else 'cpu')

        print(f"Загрузка модели на {self.device}...")
        self.model = self._load_model(model_path)
        self.model.eval()  # dropout отключен

        self.transform = self._get_transform()
        print(f"✓ Модель загружена успешно")

    def _load_model(self, model_path):
        model = models.wide_resnet50_2(weights=None)

        in_features = model.fc.in_features
        model.fc = nn.Sequential(
            nn.Dropout(DROPOUT),
            nn.Linear(in_features, self.num_classes)
        )

        model.load_state_dict(torch.load(model_path, map_location=self.device))
        model.to(self.device)

        return model

    def _get_transform(self):
        return transforms.Compose([
            transforms.Resize(256),
            transforms.CenterCrop(IMG_SIZE),
            transforms.ToTensor(),
            transforms.Normalize([0.485, 0.456, 0.406], [0.229, 0.224, 0.225])
        ])

    def predict_single(self, image_path):
        image = Image.open(image_path).convert('RGB')
        image_tensor = self.transform(image).unsqueeze(0).to(self.device)

        with torch.no_grad():
            output = self.model(image_tensor)
            _, prediction = torch.max(output, 1)

        return prediction.item()

    def predict_batch(self, image_paths):
        predictions = []

        for img_path in tqdm(image_paths, desc='Классификация'):
            pred = self.predict_single(img_path)
            predictions.append(pred)

        return predictions


def main():
    parser = argparse.ArgumentParser(description='Классификация изображений')
    parser.add_argument('path', type=str, help='Путь к директории с данными')
    args = parser.parse_args()

    test_dir = os.path.join(args.path, 'test')

    if not os.path.exists(test_dir):
        print(f"Ошибка: директория {test_dir} не найдена!")
        sys.exit(1)

    image_paths = sorted(glob(os.path.join(test_dir, '*.jpg')))

    if len(image_paths) == 0:
        print(f"Ошибка: не найдено JPG изображений в {test_dir}")
        sys.exit(1)

    print(f"Найдено {len(image_paths)} изображений")
    print("-" * 60)

    classifier = ImageClassifier('model.pth')

    print("\nЗапуск классификации...")
    predictions = classifier.predict_batch(image_paths)

    results = []
    for img_path, pred_class in zip(image_paths, predictions):
        filename = os.path.basename(img_path)
        results.append({'filenames': filename, 'label': pred_class})

    df = pd.DataFrame(results)
    df.to_csv('label_test.csv', index=False)

    print("\n" + "=" * 60)
    print(f"✓ Результаты сохранены в label_test.csv")
    print(f"Всего предсказаний: {len(predictions)}")
    print("=" * 60)

    print("\nПервые 5 предсказаний:")
    print(df.head())


if __name__ == '__main__':
    main()