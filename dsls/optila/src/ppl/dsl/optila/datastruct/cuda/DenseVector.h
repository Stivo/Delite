#ifndef _DENSEVECTOR_H_
#define _DENSEVECTOR_H_

#include <DeliteArray.h>
#include <stdio.h>

template <class T>
class DenseVector {
public:
    T *data;
    int length;
    bool isRow;

    // Constructor
    __host__ DenseVector() {
    }

    __host__ DenseVector(int _length, bool _isRow) {
        length = _length;
        isRow = _isRow;
        DeliteCudaMalloc((void**)&data,length*sizeof(T));
    }

    // Accessor Functions
    __host__ __device__ T apply(int idx) {
        return data[idx];
    }

    __host__ __device__ void update(int idx, T newVal) {
        data[idx] = newVal;
    }

    // DeliteCoolection
    __host__ __device__ int size() {
        return length;
    }

    __host__ __device__ T dcApply(int idx) {
        return data[idx];
    }

    __host__ __device__ void dcUpdate(int idx, T value) {
        data[idx] = value;
    }

    // unsafeSetData
    __host__ void unsafeSetData(DeliteArray<T> *da, int _length) {
        data = da->data;
        length = _length;
    }
    
};

#endif
