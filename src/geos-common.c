
#include <string.h>
#include "geos-common.h"

void geos_r_error_handler(const char *message, void* userdata) {
  char* errorMessage = (char*) userdata;
  unsigned long messageChars = strlen(message);
  if (messageChars >= BUFSIZ) {
    strncpy(errorMessage, message, BUFSIZ);
    errorMessage[BUFSIZ - 1] = '\0';

  } else {
    strncpy(errorMessage, message, messageChars);
    errorMessage[messageChars - 1] = '\0';
  }
}
