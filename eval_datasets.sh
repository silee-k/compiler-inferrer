#!/bin/bash
bash ./eval_dataset.sh coreutils .. '../normal_dataset/coreutils/coreutils-*x86*_O0_*'
bash ./eval_dataset.sh coreutils .. '../normal_dataset/coreutils/coreutils-*x86*_O2_*'
bash ./eval_dataset.sh coreutils .. '../normal_dataset/coreutils/coreutils-*x86*_O3_*'
bash ./eval_dataset.sh coreutils .. '../normal_dataset/coreutils/coreutils-*x86*_O1_*'