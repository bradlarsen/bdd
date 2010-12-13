import os.path as path

environments = {
    'opt' : Environment(
        CC = 'gcc', 
        CFLAGS = Split('-std=c89 -Wall -Wextra -pedantic -Wno-unused-function -O2 -DNDEBUG -Dinline=""'),
        ),
    'dbg' : Environment(
        CC = 'gcc', 
        CFLAGS = Split('-std=c89 -Wall -Wextra -pedantic -Wno-unused-function -g -Dinline=""'),
        )
    }

for env_name, env in environments.items():
    libbdd_dir = path.join('build', 'libbdd', env_name)
    env.SConscript (path.join('src', 'SConscript'),
                    exports='env',
                    variant_dir=libbdd_dir,
                    )
