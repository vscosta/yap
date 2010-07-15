# Common declarations for Unix and Windows build
LIBPL=		$(srcdir)/rdf_db.pl $(srcdir)/rdfs.pl \
		$(srcdir)/rdf_edit.pl $(srcdir)/rdf_litindex.pl \
		$(srcdir)/rdf_persistency.pl $(srcdir)/rdf_turtle.pl \
		$(srcdir)/rdf_cache.pl \
		$(srcdir)/rdf_http_plugin.pl $(srcdir)/rdf_zlib_plugin.pl \
		$(srcdir)/rdf_portray.pl \
		$(srcdir)/rdf_compare.pl $(srcdir)/turtle_base.pl \
		$(srcdir)/rdf_turtle_write.pl \
		$(srcdir)/rdf_library.pl $(srcdir)/sparql_client.pl

DATA=		$(srcdir)/rdfs.rdfs $(srcdir)/dc.rdfs $(srcdir)/eor.rdfs \
		$(srcdir)/owl.owl $(srcdir)/rdf_library.ttl
