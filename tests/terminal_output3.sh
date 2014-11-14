#! /bin/bash

echo -e "Name1 [|||||||] > |x.y|___|_x_|x.y|x.x|___|_z_|_x_|_y_|_z_|___|z.x|_x_|___|_x_| < [|||....] Name2"
sleep 1
tput cuu1
echo -e "Name1 [|||||||] > |___|_x_|x.y|x.x|___|_z_|_x_|_y_|_z_|___|z.x|_x_|___|_x_|___| < [|||....] Name2"
