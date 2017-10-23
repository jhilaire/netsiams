v_data_wos    <- isi2df(readLines(u_fpath_wosdata))
v_data_scopus <- scopus2df(readLines(u_fpath_scopusdata))


conceptualStructure(v_data_wos)
conceptualStructure(v_data_scopus)


