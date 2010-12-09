import os.path as path

environments = {
    'opt' : Environment(
        CC = 'gcc', 
        CFLAGS = Split('-std=c99 -Wall -Wextra -pedantic -O2 -DNDEBUG'),
        ),
    'dbg' : Environment(
        CC = 'gcc', 
        CFLAGS = Split('-std=c99 -Wall -Wextra -pedantic -g'),
        )
    }

for env_name, env in environments.items():
    libbdd_dir = path.join('build', 'libbdd', env_name)
    env.SConscript (path.join('src', 'SConscript'),
                    exports='env',
                    variant_dir=libbdd_dir,
                    )
