#!/bin/bash

#python train.py --data_dir data/Ghost\ Talkers --save_dir save/Ghost\ Talkers

python train.py --data_dir data/Ghost\ Talkers/Hi --save_dir save/Ghost\ Talkers/Hi
python train.py --data_dir data/Ghost\ Talkers/Med --save_dir save/Ghost\ Talkers/Med
python train.py --data_dir data/Ghost\ Talkers/Lo --save_dir save/Ghost\ Talkers/Lo