# ExtMuTgl-Monitoring

## Companion papers

Implementation from the paper:
L. Bakiri, J. Dubut, and S. Mover.
Monitoring Diameters of Causal Communication Graph.
(arxiv link soon)

Extension of the paper
C. Eberhart, J. Haydon, J. Dubut, A. Cetinkaya, and S. Pruekprasert.
Logic for Timed Agent Network Topologies.
In CDC'22, IEEE, 2022.
https://group-mmm.org/~eberhart/research/mu-tgl.pdf

## License

This project is licensed under the MIT License. See the LICENSE file for details.

## Requirements

Minimum:
- Ocaml
- Dune

For some experiments:
- Ocaml csv package
- SPACE (Swarm Planning And Control Evaluation) Simulator
https://github.com/inmo-jang/space-simulator
- Python and various requirements from the space simulator

## Commands

To run the tests:
```dune runtest```

To draw the figures from the data:
```python rv-figs.py```

## Content of the repository

- ```lib```: the source code of the monitoring
- ```bin```: various small examples
- ```test```: test files for the case study section of the paper
- ```cbba-fix```: files to replace in the space simulator to generate the desired data (positions of the agents) and to set up the simulations
- ```data.csv```: a trace of the simulator used in experiments
- ```experimentsi.csv```: monitoring data for the case study section
- ```rv-figs.py```: python script to generate the figures of the paper from the data