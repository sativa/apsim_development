#include <general\general.h>

char * GENERAL_EXPORT stristr(char *text_str, const char *srch_str);
/*	If the string 'text_str' contains a substring equal to the
 * string 'srch_str' (case insensitive), then this function will return
 * a pointer to that substring.  Otherwise, NULL will be returned.
 */

