package com.example.kotlin_auth5_klicker_fb

import android.content.Intent
import android.net.Uri
import androidx.appcompat.app.AppCompatActivity
import android.os.Bundle
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.widget.ProgressBar
import android.widget.TextView
import androidx.recyclerview.widget.LinearLayoutManager
import androidx.recyclerview.widget.RecyclerView
import kotlinx.coroutines.*
import okhttp3.OkHttpClient
import okhttp3.Request
import org.json.JSONArray
import java.net.URL
import android.widget.Toast

class RiverListActivity : AppCompatActivity() {

    private lateinit var recyclerView: RecyclerView
    private lateinit var progressBar: ProgressBar
    private lateinit var errorTextView: TextView
    private val adapter = RiverAdapter(emptyList())
    private val scope = CoroutineScope(Dispatchers.Main + Job())

    private val RIVERS_API_URL = "https://mysafeinfo.com/api/data?list=riverseurope&format=json"

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_river_list)

        recyclerView = findViewById(R.id.rv_river_list)
        progressBar = findViewById(R.id.progressBar)
        errorTextView = findViewById(R.id.tv_error)

        recyclerView.layoutManager = LinearLayoutManager(this)
        recyclerView.adapter = adapter


        loadRiversFromNetwork()
    }

    private fun loadRiversFromNetwork() {
        scope.launch {
            showLoading(true)

            try {
                val rivers = withContext(Dispatchers.IO) {
                    fetchRiversFromApi()
                }

                if (rivers.isEmpty()) {
                    showError("Нет данных для отображения")
                } else {
                    adapter.updateData(rivers)
                    showData()
                }
            } catch (e: Exception) {
                showError("Ошибка загрузки: ${e.message}")
                e.printStackTrace()
            }
        }
    }

    private fun fetchRiversFromApi(): List<River> {
        val rivers = mutableListOf<River>()

        try {
            val client = OkHttpClient.Builder()
                .connectTimeout(15, java.util.concurrent.TimeUnit.SECONDS)
                .readTimeout(15, java.util.concurrent.TimeUnit.SECONDS)
                .build()

            val request = Request.Builder()
                .url(RIVERS_API_URL)
                .addHeader("User-Agent", "Android App")
                .build()

            val response = client.newCall(request).execute()

            if (!response.isSuccessful) {
                throw Exception("HTTP ошибка: ${response.code}")
            }

            val jsonString = response.body?.string()
            if (jsonString.isNullOrEmpty()) {
                throw Exception("Пустой ответ от сервера")
            }

            val jsonArray = JSONArray(jsonString)

            for (i in 0 until jsonArray.length()) {
                val obj = jsonArray.getJSONObject(i)
                val river = River(
                    id = obj.getInt("ID"),
                    name = obj.getString("River"),
                    url = obj.getString("Url"),
                    kilometers = obj.getInt("Kilometers"),
                    miles = obj.getInt("Miles")
                )
                rivers.add(river)
            }

        } catch (e: Exception) {
            throw Exception("Не удалось загрузить данные: ${e.message}")
        }

        return rivers
    }

    private fun showLoading(show: Boolean) {
        progressBar.visibility = if (show) View.VISIBLE else View.GONE
        if (show) {
            recyclerView.visibility = View.GONE
            errorTextView.visibility = View.GONE
        }
    }

    private fun showError(message: String) {
        errorTextView.text = message
        errorTextView.visibility = View.VISIBLE
        recyclerView.visibility = View.GONE
        progressBar.visibility = View.GONE
    }

    private fun showData() {
        recyclerView.visibility = View.VISIBLE
        errorTextView.visibility = View.GONE
        progressBar.visibility = View.GONE
    }

    override fun onDestroy() {
        super.onDestroy()
        scope.cancel()
    }
}

class RiverAdapter(private var rivers: List<River>) :
    RecyclerView.Adapter<RiverAdapter.ViewHolder>() {

    class ViewHolder(view: View) : RecyclerView.ViewHolder(view) {
        val nameTextView: TextView = view.findViewById(R.id.tv_river_name)
        val detailsTextView: TextView = view.findViewById(R.id.tv_river_details)
        val urlTextView: TextView = view.findViewById(R.id.tv_river_url)
    }

    override fun onCreateViewHolder(parent: ViewGroup, viewType: Int): ViewHolder {
        val view = LayoutInflater.from(parent.context)
            .inflate(R.layout.item_river, parent, false)
        return ViewHolder(view)
    }

    override fun onBindViewHolder(holder: ViewHolder, position: Int) {
        val river = rivers[position]

        holder.nameTextView.text = "${river.id}. ${river.name}"
        holder.detailsTextView.text = "Длина: ${river.kilometers} км (${river.miles} миль)"
        holder.urlTextView.text = "Ссылка: ${river.url}"


        holder.itemView.setOnClickListener {
            try {
                val intent = Intent(Intent.ACTION_VIEW, Uri.parse(river.url))
                holder.itemView.context.startActivity(intent)
            } catch (e: Exception) {
                Toast.makeText(holder.itemView.context,
                    "Не удалось открыть ссылку", Toast.LENGTH_SHORT).show()
            }
        }
    }

    override fun getItemCount() = rivers.size

    fun updateData(newRivers: List<River>) {
        rivers = newRivers
        notifyDataSetChanged()
    }
}