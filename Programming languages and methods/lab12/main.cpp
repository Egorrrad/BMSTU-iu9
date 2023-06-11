#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <algorithm>
#include <filesystem>

using namespace std;
namespace fs = std::filesystem;

bool isJavaFile(const fs::directory_entry& entry) {
    return entry.path().extension() == ".java";
}

vector<string> getImports(const string& filename) {
    vector<string> imports;
    ifstream file(filename);
    if (!file.is_open()) {
        cerr << "Error opening file: " << filename << endl;
        return imports;
    }
    string line;
    while (getline(file, line)) {
        if (line.find("import") == 0) {
            int start = line.find(" ") + 1;
            int end = line.find(";", start);
            string import = line.substr(start, end - start);
            imports.push_back(import);
        }
    }
    file.close();
    return imports;
}

int main() {
    cout << "Enter directory path: ";
    string path;
    getline(cin, path);

    int filesCount = 0;
    vector<string> allImports;
    for (const auto& entry : fs::directory_iterator(path)) {
        if (isJavaFile(entry)) {
            filesCount++;
            vector<string> imports = getImports(entry.path().string());
            allImports.insert(allImports.end(), imports.begin(), imports.end());
        }
    }

    ofstream outfile("imports.txt");
    if (!outfile.is_open()) {
        cerr << "Error creating file: imports.txt" << endl;
        return 1;
    }

    sort(allImports.begin(), allImports.end());
    string currentImport = "";
    int count = 0;
    for (const auto& import : allImports) {
        if (import != currentImport) {
            if (count > 0) {
                outfile << " - " << count << endl;
            }
            currentImport = import;
            count = 1;
            outfile << currentImport;
        } else {
            count++;
        }
    }
    if (count > 0) {
        outfile << " - " << count << endl;
    }

    outfile.close();
    cout << "Found .java files: " << filesCount << endl;
    cout << "imports.txt file successfully created." << endl;

    return 0;
}



