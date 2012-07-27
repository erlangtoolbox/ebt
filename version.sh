if [ -z $BUILD_NUMBER ]
then
	echo -n 0.1.0
else
	echo -n 0.1.0.$BUILD_NUMBER
fi
