#include <emmintrin.h>

void add_i32(__m128i vr_128[], const __m128i v1_128[], const __m128i v2_128[], const int n) {
  for(int i = 0; i < n; i++){
    vr_128[i] = _mm_add_epi32(v1_128[i], v2_128[i]);
  }
}

void add_i32_alt(int32_t vr_32[], const int32_t v1_32[], const int32_t v2_32[], const int n) {
  __m128i* vr_128 = (__m128i*) vr_32;
  __m128i* v1_128 = (__m128i*) v1_32;
  __m128i* v2_128 = (__m128i*) v2_32;
  for(int i = 0; i < n; i++){
    _mm_store_si128(&vr_128[i], _mm_add_epi32(_mm_load_si128(&v1_128[i]), _mm_load_si128(&v2_128[i])));
  }
}

void add_i32_simple(int32_t vr_32[], const int32_t v1_32[], const int32_t v2_32[], const int n) {
  for(int i = 0; i < n; i++){
    vr_32[i] = v1_32[i] + v2_32[i];
  }
}

void add_i32_full(int32_t vr_32[], const int32_t v1_32[], const int32_t v2_32[], const int n) {
  int i = 0;
  __m128i* vr_128 = (__m128i*) vr_32;
  __m128i* v1_128 = (__m128i*) v1_32;
  __m128i* v2_128 = (__m128i*) v2_32;
  for(; i < n / 4; i++){
    _mm_store_si128(&vr_128[i], _mm_add_epi32(_mm_load_si128(&v1_128[i]), _mm_load_si128(&v2_128[i])));
  }
  for(; i < n; i++){
    vr_32[i] = v1_32[i] + v2_32[i];
  }
}
