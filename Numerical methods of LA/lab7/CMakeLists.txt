cmake_minimum_required(VERSION 3.16)
project(Lapack)

set(OPENBLAS_DIR "/opt/homebrew/opt/openblas")

include_directories(${OPENBLAS_DIR}/include)

add_executable(${PROJECT_NAME} main.cpp)

target_link_libraries(${PROJECT_NAME} ${OPENBLAS_DIR}/lib/libopenblas.dylib)