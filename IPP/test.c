/*
 * Dynamic loading + use of the libnotify library.
 *
 * Steve
 * --
 * http://www.steve.org.uk/
 */
#include <stdlib.h>
#include <stdio.h>
#include <dlfcn.h>

int main( int argc, char *argv[] )
{
  /* Library + notification handles */
  void *handle, *n;

  /* signatures of functions we're going to invoke dynamically. */
  typedef void  (*notify_init_t)(char *);
  typedef void *(*notify_notification_new_t)( char *, char *, char *, char *);
  typedef void  (*notify_notification_set_timeout_t)( void *, int );
  typedef void (*notify_notification_show_t)(void *, char *);

  /* open the library */
  handle= dlopen("libnotify.so.1", RTLD_LAZY);
  if ( handle == NULL )
  {
    printf("Failed to open library\n" );
    return 1;
  }

  /* Find the notify_init function and invoke it. */
  notify_init_t init = (notify_init_t)dlsym(handle, "notify_init");
  if ( init == NULL  )
  {
    printf("Library function not found: notify_init\n");
    dlclose( handle );
    return 1;
  }
  init("Basics");


  /* Find the notify_notification_new function, and invoke it. */
  notify_notification_new_t nnn = (notify_notification_new_t)dlsym(handle, "notify_notification_new");
  if ( nnn == NULL  )
  {
    printf("Library function not found: notify_notification_new\n");
    dlclose( handle );
    return 1;
  }
  n = nnn("Test subject", "Test body with <b>bold</b>, and <i>italic</i>!", NULL, NULL);

  /* Find the notify_notification_set_timeout function and invoke it. */
  notify_notification_set_timeout_t nnst = (notify_notification_set_timeout_t)dlsym(handle, "notify_notification_set_timeout");
  if ( nnst == NULL  )
    {
      printf("Library function not found: notify_notification_set_timeout\n");
      dlclose( handle );
      return 1;
    }
  /* invoke function, 3 second timeout. */
  nnst(n, 3000 );


  /* Finally shpow the notification. */
  notify_notification_show_t show = (notify_notification_show_t)dlsym(handle, "notify_notification_show");
  if ( init == NULL  )
  {
    printf("Library function not found: notify_notification_show\n");
    dlclose( handle );
    return 1;
  }
  /* invoke function, passing value of integer as a parameter */
  show(n, NULL );

  /* close the library and exit*/
  dlclose(handle );
  return 0;
}
