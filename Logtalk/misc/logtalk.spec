Name: logtalk
Summary: Logtalk - Open source object-oriented extension to Prolog
Version: 2.17.1
Release: 1
License: Artistic License 2.0
Group: Development/Languages
Packager: Paulo Moura <pmoura@logtalk.org>
Source: http://www.logtalk.org/files/lgt2171.tar.gz
BuildArchitectures: noarch
URL: http://www.logtalk.org/
Prefix: /usr/local
AutoReqProv: no
%description
Logtalk is an open source object-oriented extension to the Prolog programming language. Integrating logic programming with object-oriented and event-driven programming, it is compatible with most Prolog compilers. It supports both prototypes and classes. In addition, it supports component-based programming through category-based composition.
%prep
%setup -n lgt2171
%build
%install
rm -rf /usr/local/lgt2171
rm -f /usr/local/logtalk
mkdir /usr/local/lgt2171
cp -R * /usr/local/lgt2171
cd /usr/local
chmod -R go-w,a+r lgt2171
chmod a+x lgt2171
chmod a+x lgt2171/misc/*.sh
chmod a+x lgt2171/xml/*.sh
ln -sf lgt2171 logtalk
cd bin
ln -sf ../lgt2171/misc/cplgtdirs.sh cplgtdirs.sh
%clean
%files
%defattr(-,root,users)
%doc /usr/local/lgt2171/BIBLIOGRAPHY
%doc /usr/local/lgt2171/INSTALL
%doc /usr/local/lgt2171/LICENSE
%doc /usr/local/lgt2171/QUICK_START
%doc /usr/local/lgt2171/README
%doc /usr/local/lgt2171/RELEASE_NOTES
%doc /usr/local/lgt2171/UPGRADING
/usr/local/lgt2171/compiler
/usr/local/lgt2171/configs
/usr/local/lgt2171/examples
/usr/local/lgt2171/library
%docdir /usr/local/lgt2171/manuals
/usr/local/lgt2171/manuals
/usr/local/lgt2171/misc
/usr/local/lgt2171/wenv
/usr/local/lgt2171/xml
/usr/local/logtalk
/usr/local/bin/cplgtdirs.sh
