Name: logtalk
Summary: Logtalk - Open source object-oriented extension to Prolog
Version: 2.22.1
Release: 1
License: Artistic License 2.0
Group: Development/Languages
Packager: Paulo Moura <pmoura@logtalk.org>
Source: http://www.logtalk.org/files/lgt2221.tgz
BuildArchitectures: noarch
URL: http://www.logtalk.org/
Prefix: /usr/local
AutoReqProv: no
%description
Logtalk is an open source object-oriented extension to the Prolog programming language. Integrating logic programming with object-oriented and event-driven programming, it is compatible with most Prolog compilers. It supports both prototypes and classes. In addition, it supports component-based programming through category-based composition.
%prep
%setup -n lgt2221
%build
%install
rm -rf /usr/local/lgt2221
rm -f /usr/local/logtalk
mkdir /usr/local/lgt2221
cp -R * /usr/local/lgt2221
cd /usr/local
chmod -R go-w,a+r lgt2221
chmod a+x lgt2221
chmod a+x lgt2221/misc/*.sh
chmod a+x lgt2221/xml/*.sh
ln -sf lgt2221 logtalk
cd bin
ln -sf ../lgt2221/misc/cplgtdirs.sh cplgtdirs
ln -sf ../lgt2221/xml/lgt2pdf.sh lgt2pdf
ln -sf ../lgt2221/xml/lgt2html.sh lgt2html
%clean
%files
%defattr(-,root,users)
%doc /usr/local/lgt2221/BIBLIOGRAPHY
%doc /usr/local/lgt2221/INSTALL
%doc /usr/local/lgt2221/LICENSE
%doc /usr/local/lgt2221/QUICK_START
%doc /usr/local/lgt2221/README
%doc /usr/local/lgt2221/RELEASE_NOTES
%doc /usr/local/lgt2221/UPGRADING
/usr/local/lgt2221/compiler
/usr/local/lgt2221/configs
/usr/local/lgt2221/examples
/usr/local/lgt2221/libpaths
/usr/local/lgt2221/library
%docdir /usr/local/lgt2221/manuals
/usr/local/lgt2221/manuals
/usr/local/lgt2221/misc
/usr/local/lgt2221/wenv
/usr/local/lgt2221/xml
/usr/local/logtalk
/usr/local/bin/cplgtdirs
/usr/local/bin/lgt2pdf
/usr/local/bin/lgt2html
