Name: logtalk
Summary: Logtalk - Open source object-oriented extension to Prolog
Version: 2.17.2
Release: 1
License: Artistic License 2.0
Group: Development/Languages
Packager: Paulo Moura <pmoura@logtalk.org>
Source: http://www.logtalk.org/files/lgt2172.tar.gz
BuildArchitectures: noarch
URL: http://www.logtalk.org/
Prefix: /usr/local
AutoReqProv: no
%description
Logtalk is an open source object-oriented extension to the Prolog programming language. Integrating logic programming with object-oriented and event-driven programming, it is compatible with most Prolog compilers. It supports both prototypes and classes. In addition, it supports component-based programming through category-based composition.
%prep
%setup -n lgt2172
%build
%install
rm -rf /usr/local/lgt2172
rm -f /usr/local/logtalk
mkdir /usr/local/lgt2172
cp -R * /usr/local/lgt2172
cd /usr/local
chmod -R go-w,a+r lgt2172
chmod a+x lgt2172
chmod a+x lgt2172/misc/*.sh
chmod a+x lgt2172/xml/*.sh
ln -sf lgt2172 logtalk
cd bin
ln -sf ../lgt2172/misc/cplgtdirs.sh cplgtdirs.sh
%clean
%files
%defattr(-,root,users)
%doc /usr/local/lgt2172/BIBLIOGRAPHY
%doc /usr/local/lgt2172/INSTALL
%doc /usr/local/lgt2172/LICENSE
%doc /usr/local/lgt2172/QUICK_START
%doc /usr/local/lgt2172/README
%doc /usr/local/lgt2172/RELEASE_NOTES
%doc /usr/local/lgt2172/UPGRADING
/usr/local/lgt2172/compiler
/usr/local/lgt2172/configs
/usr/local/lgt2172/examples
/usr/local/lgt2172/library
%docdir /usr/local/lgt2172/manuals
/usr/local/lgt2172/manuals
/usr/local/lgt2172/misc
/usr/local/lgt2172/wenv
/usr/local/lgt2172/xml
/usr/local/logtalk
/usr/local/bin/cplgtdirs.sh
