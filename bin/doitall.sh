#! /bin/bash
bin/roxy -p "." --clean
bin/buildpackage.sh
bin/checkpackage.sh
bin/installpackage.sh
