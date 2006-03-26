Name: logtalk
Summary: Logtalk - Open source object-oriented extension to Prolog
Version: 2.27.1
Release: 1
License: Artistic License 2.0
Group: Development/Languages
Packager: Paulo Moura <pmoura@logtalk.org>
Source: http://www.logtalk.org/files/lgt2271.tgz
BuildArchitectures: noarch
URL: http://www.logtalk.org/
Prefix: /usr/local
AutoReqProv: no
%description
Logtalk is an open source object-oriented extension to the Prolog programming language. Integrating logic programming with object-oriented and event-driven programming, it is compatible with most Prolog compilers. It supports both prototypes and classes. In addition, it supports component-based programming through category-based composition.
%prep
%setup -n lgt2271
%build
%install
rm -rf /usr/local/lgt2271
rm -f /usr/local/logtalk
mkdir /usr/local/lgt2271
cp -R * /usr/local/lgt2271
cd /usr/local
chmod -R go-w,a+r lgt2271
chmod a+x lgt2271
chmod a+x lgt2271/scripts/*.sh
chmod a+x lgt2271/xml/*.sh
ln -sf lgt2271 logtalk
cd bin
ln -sf ../logtalk/scripts/cplgtdirs.sh cplgtdirs
ln -sf ../logtalk/xml/lgt2pdf.sh lgt2pdf
ln -sf ../logtalk/xml/lgt2html.sh lgt2html
ln -sf ../logtalk/xml/lgt2xml.sh lgt2xml
%clean
%files
%defattr(-,root,users)
%doc /usr/local/lgt2271/BIBLIOGRAPHY
%doc /usr/local/lgt2271/INSTALL
%doc /usr/local/lgt2271/LICENSE
%doc /usr/local/lgt2271/QUICK_START
%doc /usr/local/lgt2271/README
%doc /usr/local/lgt2271/RELEASE_NOTES
%doc /usr/local/lgt2271/UPGRADING
/usr/local/lgt2271/compiler
/usr/local/lgt2271/configs
/usr/local/lgt2271/contributions
/usr/local/lgt2271/examples
/usr/local/lgt2271/libpaths
/usr/local/lgt2271/library
%docdir /usr/local/lgt2271/manuals
/usr/local/lgt2271/manuals
/usr/local/lgt2271/scripts
/usr/local/lgt2271/wenv
/usr/local/lgt2271/xml
/usr/local/logtalk
/usr/local/bin/cplgtdirs
/usr/local/bin/lgt2pdf
/usr/local/bin/lgt2html
/usr/local/bin/lgt2xml
