#### Collaboration Networks ############
net <- bibliometrix::biblioNetwork(v_data_wos, analysis = "collaboration", network="authors")
networkPlot(net, n=20, type = "kamada", Title = "Collaboration - authors")
net <- bibliometrix::biblioNetwork(v_data_wos, analysis = "collaboration", network="universities")
networkPlot(net, n=20, type = "kamada", Title = "Collaboration - universities")
net <- bibliometrix::biblioNetwork(v_data_wos, analysis = "collaboration", network="countries")
networkPlot(net, n=10, type = "kamada", Title = "Collaboration - countries")


#### Co-citation Networks ##############
net <- bibliometrix::biblioNetwork(v_data_wos, analysis = "co-citation", network="authors")
networkPlot(net, n=20, type = "kamada", Title = "Co-citation - authors")
net <- bibliometrix::biblioNetwork(v_data_wos, analysis = "co-citation", network="references")
networkPlot(net, n=20, type = "kamada", Title = "Co-citation - references")
net <- bibliometrix::biblioNetwork(v_data_wos, analysis = "co-citation", network="sources")
networkPlot(net, n=20, type = "kamada", Title = "Co-citation - sources")


#### Coupling Networks ################
net <- bibliometrix::biblioNetwork(v_data_wos, analysis = "coupling", network="references")
networkPlot(net, n=69, type = "kamada", Title = "Coupling - references")
net <- bibliometrix::biblioNetwork(v_data_wos, analysis = "coupling", network="authors")
networkPlot(net, n=20, type = "kamada", Title = "Coupling - authors")
net <- bibliometrix::biblioNetwork(v_data_wos, analysis = "coupling", network="sources")
networkPlot(net, n=10, type = "kamada", Title = "Coupling - sources")
#net <- bibliometrix::biblioNetwork(v_data_wos, analysis = "coupling", network="countries")
#networkPlot(net, n=10, type = "kamada", Title = "Coupling - countries")


#### Co-occurrence Networks ############
net <- bibliometrix::biblioNetwork(v_data_wos, analysis = "co-occurrences", network="authors")
networkPlot(net, n=20, type = "kamada", Title = "Co-occurrences - authors")
net <- bibliometrix::biblioNetwork(v_data_wos, analysis = "co-occurrences", network="sources")
networkPlot(net, n=20, type = "kamada", Title = "Co-occurrences - sources")
net <- bibliometrix::biblioNetwork(v_data_wos, analysis = "co-occurrences", network="keywords")
networkPlot(net, n=20, type = "kamada", Title = "Co-occurrences - keywords")
net <- bibliometrix::biblioNetwork(v_data_wos, analysis = "co-occurrences", network="author_keywords")
networkPlot(net, n=20, type = "kamada", Title = "Co-occurrences - author_keywords")
net <- bibliometrix::biblioNetwork(v_data_wos, analysis = "co-occurrences", network="titles")
networkPlot(net, n=20, type = "kamada", Title = "Co-occurrences - titles")
net <- bibliometrix::biblioNetwork(v_data_wos, analysis = "co-occurrences", network="abstracts")
networkPlot(net, n=20, type = "kamada", Title = "Co-occurrences - abstracts")