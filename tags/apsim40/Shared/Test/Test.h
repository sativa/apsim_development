#ifndef TestH
#define TestH

#define test(cond) internalTest(cond, __FILE__, __LINE__)

void internalTest(bool condition, const char* fileName, unsigned line);

#endif