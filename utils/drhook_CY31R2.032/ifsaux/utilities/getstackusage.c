#ifdef VPP
long long int getstackusage_dummy_() { return 0L; }
#else
long long int getstackusage_() { return 0L; }
#endif
