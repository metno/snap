/* dummy _TraceCalls */
void _TraceCalls(char *nothing) { }

#if defined(NEC)

/* some dummies for NEC + libmpi_serial */

double mpi_nec_block_time_() { return 0; }
void mpi_type_dup_fn_() { }
void mpi_type_null_copy_fn_() { }
void mpi_type_null_delete_fn_() { }
void mpi_comm_dup_fn_() { }
void mpi_comm_null_copy_fn_() { }
void mpi_comm_null_delete_fn_() { }
void mpi_win_dup_fn_() { }
void mpi_win_null_copy_fn_() { }
void mpi_win_null_delete_fn_() { }

#endif

