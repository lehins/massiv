#ifndef MASSIV_INCLUDE

#define MASSIV_INCLUDE

#if MASSIV_UNSAFE_CHECKS
#define INDEX_CHECK(name, s, f) (indexWith __FILE__ __LINE__ (name) (s) (f))
#else
#define INDEX_CHECK(name, s, f) ((f))
#endif

#endif
