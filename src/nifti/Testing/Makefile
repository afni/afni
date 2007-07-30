
TAR     ?= /bin/tar
RM      ?= /bin/rm

# get the regression testing data tree
regress_data: nifti.regress_test/nifti.regress.data

# remove any result directories
regress_clean:
	$(RM) -fr nifti.regress_test/results*

# remove any result directories, and remove the data tree
regress_clean_all: regress_clean
	$(RM) -fr nifti.regress_test/nifti.regress.data

# download and unpack the regression data tree
nifti.regress_test/nifti.regress.data:
	wget http://nifti.nimh.nih.gov/pub/dist/data/nifti.regress.data.tgz
	( cd nifti.regress_test; $(TAR) -xzf ../nifti.regress.data.tgz; )
	$(RM) nifti.regress.data.tgz

