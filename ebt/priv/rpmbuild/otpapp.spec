
%description
@DESCRIPTION@

%install
APPNAME=`basename %{_builddir}`
ERLANG_LIB=/usr/%{_lib}/erlang/lib/$APPNAME
TARGET=%{buildroot}$ERLANG_LIB

mkdir -p $TARGET
for f in %{_builddir}/*
do
    cp -r $f $TARGET
done

if [ -d  %{_builddir}/bin ]
then
    mkdir -p %{buildroot}%{_bindir}
    for f in %{_builddir}/bin/*
    do
        FILE=`basename $f`
        ln -s $ERLANG_LIB/bin/$FILE %{buildroot}%{_bindir}/$FILE
    done
fi

if [ -d  %{_builddir}/etc ]
then
    mkdir -p %{buildroot}%{_sysconfdir}
    for f in %{_builddir}/etc/*
    do
        cp -r $f %{buildroot}%{_sysconfdir}
    done
fi

%files
/

%changelog
