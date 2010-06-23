# Makefile fragments to are updated frequently and can be shared

LIBPL=		$(srcdir)/html_write.pl $(srcdir)/http_client.pl \
		$(srcdir)/http_header.pl \
		$(srcdir)/http_mime_plugin.pl $(srcdir)/http_sgml_plugin.pl \
		$(srcdir)/mimepack.pl $(srcdir)/mimetype.pl \
		$(srcdir)/dcg_basics.pl \
		$(srcdir)/thread_httpd.pl $(srcdir)/xpce_httpd.pl \
		$(srcdir)/inetd_httpd.pl \
		$(srcdir)/http_wrapper.pl $(srcdir)/http_open.pl \
		$(srcdir)/http_session.pl \
		$(srcdir)/http_error.pl $(srcdir)/http_parameters.pl \
		$(srcdir)/http_dispatch.pl \
		$(srcdir)/http_authenticate.pl $(srcdir)/http_stream.pl \
		$(srcdir)/http_log.pl \
		$(srcdir)/http_path.pl $(srcdir)/http_hook.pl \
		$(srcdir)/html_head.pl $(srcdir)/http_exception.pl \
		$(srcdir)/json.pl $(srcdir)/http_json.pl \
		$(srcdir)/json_convert.pl $(srcdir)/http_dirindex.pl \
		$(srcdir)/http_server_files.pl $(srcdir)/http_pwp.pl \
		$(srcdir)/http_host.pl \
		$(srcdir)/http_openid.pl $(srcdir)/js_write.pl
EXAMPLES=	$(srcdir)/demo_body.pl $(srcdir)/demo_client.pl \
		$(srcdir)/demo_threads.pl $(srcdir)/demo_xpce.pl \
		$(srcdir)/calc.pl $(srcdir)/demo_files.pl \
		$(srcdir)/demo_pwp.pl $(srcdir)/demo_openid.pl
EXAMPLEEXE=	demo_inetd
XPCEPL=		$(srcdir)/http_image.pl
