

#ifndef LAB9_SERIES_H
#define LAB9_SERIES_H
#include <functional>

using namespace std;
template <typename T>

class Series{
public:
    using func_t = function<T(T)>;

private:
    func_t f;
    int i0;

public:
    Series(func_t func) : f(func){
    };
    void setI(int i){
        i0=i;
    }
    Series<T> operator+(const Series<T> &other) const {
        return Series<T>([this, &other](T t) {
            return f(t) + other.f(t);
        });
    }
    Series<T> operator*(T k) const {
        return Series<T>([this, k](T t) {
            return k * f(t);
        });
    }

    T operator()(int i){
        if (i<i0){
            return 0;
        }
        return f(i);
    }
};


#endif //LAB9_SERIES_H
