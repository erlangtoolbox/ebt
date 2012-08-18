VSN=1.0.1
if [ -z $BUILD_NUMBER ]
then
	echo -n $VSN
else
	echo -n $VSN.$BUILD_NUMBER
fi
