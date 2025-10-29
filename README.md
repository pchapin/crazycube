
CrazyCube
=========

This repository is a conceptual fork of the CrazyCube repository in Vermont State University's
CubeSat Laboratory organization [here](https://github.com/cubesatlab/crazycube). By "conceptual"
I mean that this repository is *not* a fork in the usual Git sense. It is entirely independent
and was created by importing the original repository rather than "forking" it. That import was
done on October 29, 2025.

Peter Chapin  
spicacality@kelseymountain.org  


CubedOS flight software for the Crazyflie. This repository includes the CubedOS repository as a
submodule. The CubedOS repository includes the Ada Drivers Library (ADL) as a submodule. To
ensure that all files are available as intended, this repository should be cloned using a
command such as:

    $ git clone --recurse-submodules https://github.com/cubesatlab/crazycube.git <path>
    
Where `<path>` should be replaced with the location of where you want the CrazyCube repository
to be placed.

If you forget the `--recurse-submodules` option on the clone, you can initialize and clone the
submodules in a separate step using a command such as:

    $ git submodule update --init --recursive

