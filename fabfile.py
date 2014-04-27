from fabric.api import cd, run, sudo, env, roles, execute
from datetime import datetime

env.roledefs = {
    'webuser': ['bloge@andrewlorente.com'],
    'sudoer': ['alorente@andrewlorente.com'],
}
env.hosts = ['andrewlorente.com']

def deploy():
    release_id = datetime.now().strftime("%Y%m%d%H%M%S")
    execute(build, release_id)
    release(release)

@roles('webuser')
def build(release_id):
    releases_dir = "/u/apps/bloge/releases/"
    run("git clone -q https://github.com/AndrewLorente/bloge.git " +
        releases_dir + release_id)
    with cd(releases_dir + release_id):
        run("cabal update")
        run("cabal install --constraint 'template-haskell installed' --dependencies-only --force-reinstall -v")
        run("cabal configure")
        run("cabal build")
    run("ln -nfs /u/apps/bloge/releases/{0} "
        "/u/apps/bloge/current".format(release_id))

@roles('sudoer')
def release(*args):
    sudo("initctl restart bloge")

