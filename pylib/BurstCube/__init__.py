import os
from pkg_resources import resource_filename

__version__ = "unknown"

try:
    from .version import get_git_version
    __version__ = get_git_version()
except Exception as message:
    print(message)

__author__ = "Jeremy Perkins"

os.environ['BCTEST'] = resource_filename('BurstCube', '/data')
