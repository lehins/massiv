#ifndef MASSIV_INCLUDE

#define MASSIV_INCLUDE

#if MASSIV_UNSAFE_CHECKS
#define HAS_CALL_STACK (HasCallStack)
#else
#define HAS_CALL_STACK ()
#endif

#endif
