# SCANPLOT - Um sistema de plotagem simples para o SCANTEC
# Copyright (C) 2020 INPE
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

from setuptools import setup, find_packages

with open("README.md", "r") as fh:
    long_description = fh.read()

setup(
    name="SCANPLOT", 
    description="Um sistema de plotagem simples para o SCANTEC",
    version="1.1.0",
    author="Carlos Frederico Bastarz",
    author_email="cfbastarz@gmail.com",
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://github.com/cfbastarz/SCANPLOT",
    packages=find_packages(include=['.']),
    install_requires=['numpy','matplotlib','xarray','pandas','seaborn','SkillMetrics','scipy'],
    classifiers=[
        "Development Status :: 3 - Alpha",
        "Programming Language :: Python :: 3",
        "License :: OSI Approved :: GNU General Public License v3 (GPLv3)",
        "Operating System :: POSIX :: Linux",
    ],
    python_requires='>=3.8.2',
)
