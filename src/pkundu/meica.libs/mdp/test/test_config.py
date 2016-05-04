"""Test the configuration object."""

from mdp import config

class TestConfig(object):
    def teardown_method(self, method):
        delattr(config, 'has_test_property')

    def test_config_depfound(self):
        s = config.ExternalDepFound('test_property', 0.777)
        assert bool(s) == True
        assert config.has_test_property
        info = config.info()
        assert 'test property' in info
        assert '0.777' in info

    def test_config_depfound_string(self):
        s = config.ExternalDepFound('test_property', '0.777')
        assert bool(s) == True
        assert config.has_test_property
        info = config.info()
        assert 'test property' in info
        assert '0.777' in info

    def test_config_depfailed_exc(self):
        s = config.ExternalDepFailed('test_property', ImportError('GOOGOO'))
        assert bool(s) == False
        assert not config.has_test_property
        info = config.info()
        assert 'test property' in info
        assert 'GOOGOO' in info

    def test_config_depfailed_string(self):
        s = config.ExternalDepFailed('test_property', 'GOOGOO')
        assert bool(s) == False
        assert not config.has_test_property
        info = config.info()
        assert 'test property' in info
        assert 'GOOGOO' in info
