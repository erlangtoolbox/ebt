
%description
@DESCRIPTION@

%define _erlang_lib /usr/%{_lib}/erlang/lib/@APPNAME@
%install

TARGET=%{buildroot}%{_erlang_lib}
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
        ln -s %{_erlang_lib}/bin/$FILE %{buildroot}%{_bindir}/$FILE
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
