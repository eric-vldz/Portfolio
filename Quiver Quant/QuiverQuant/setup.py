from setuptools import setup, find_packages

setup(
    name='live_congress_trading',
    version='0.1',
    packages=find_packages(),
    install_requires=[
        'requests',
        'pandas'
    ],
)