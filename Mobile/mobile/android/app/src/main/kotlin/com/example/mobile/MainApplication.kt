package com.example.mobile

import android.app.Application
import com.yandex.mapkit.MapKitFactory

class MainApplication : Application() {
    override fun onCreate() {
        super.onCreate()
        // здесь указывается ключ от яндекс карт
        MapKitFactory.setApiKey(key)
    }
}