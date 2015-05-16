
#ifndef QUARRY_EXPORT_H
#define QUARRY_EXPORT_H

#ifdef QUARRY_STATIC_DEFINE
#  define QUARRY_EXPORT
#  define QUARRY_NO_EXPORT
#else
#  ifndef QUARRY_EXPORT
#    ifdef quarry_EXPORTS
        /* We are building this library */
#      define QUARRY_EXPORT 
#    else
        /* We are using this library */
#      define QUARRY_EXPORT 
#    endif
#  endif

#  ifndef QUARRY_NO_EXPORT
#    define QUARRY_NO_EXPORT 
#  endif
#endif

#ifndef QUARRY_DEPRECATED
#  define QUARRY_DEPRECATED 
#endif

#ifndef QUARRY_DEPRECATED_EXPORT
#  define QUARRY_DEPRECATED_EXPORT QUARRY_EXPORT QUARRY_DEPRECATED
#endif

#ifndef QUARRY_DEPRECATED_NO_EXPORT
#  define QUARRY_DEPRECATED_NO_EXPORT QUARRY_NO_EXPORT QUARRY_DEPRECATED
#endif

#define DEFINE_NO_DEPRECATED 0
#if DEFINE_NO_DEPRECATED
# define QUARRY_NO_DEPRECATED
#endif

#endif
