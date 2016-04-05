Title: Signing Your Git Commits With GPG
Posted: 2016-04-05T11:15:00-07:00
Tags:
    git
    pgp

Github has just announced [GPG signature verification](https://github.com/blog/2144-gpg-signature-verification), which helps verify that commits made in someone's name were indeed made by that person. It's long been a dirty secret of git that [you can impersonate anyone with minimal effort](http://www.jayhuang.org/blog/pushing-code-to-github-as-linus-torvalds/), so GPG verification adds a nice layer of assurance. Now that I've gone through the hassle of setting up automatic commit signatures, here's how you can do it too.

--------------------8<------------------

Before you can sign keys with GPG, you'll need a GPG keypair. The ins and outs of GPG key generation are nothing I want to write about, but [this guide for Ubuntu](https://help.ubuntu.com/community/GnuPrivacyGuardHowto) should give you what you need (it'll behave the same on OSX, except you'll have to install GPG yourself).

Once you have a keypair, you're ready to start signing. The simplest way to sign a commit with git is with `git commit --gpg-sign`, but adding that option manually will get real old real fast. Instead, you can configure git to sign _all_ commits: `git config --global commit.gpgsign true` (yes, it's `gpg-sign` at the command line and `gpgsign` in your config). At this point, if your committer email matches the email in your gpg key, you're off and running. All your commits will be signed and they'll show up as verified on Github.

![committing with --gpg-sign](https://cdn.erincall.com/9b3ee7e9db7ea445d56df479ed10859d43b717e2)

There's a fly in the ointment, though: you now have to type your GPG passphrase every time you commit. However, GPG ships with a background daemon to solve this problem, and despite being a GPG tool it's not too hard to get it running. Have some Fish (or [Bash, if you must](https://github.com/ErinCall/Dotfiles/blob/master/.bashrc#L32-L40)):

```Fish
if not begin
    # Is the agent running already? Does the agent-info file exist, and if so,
    # is there a process with the pid given in the file?
    [ -f ~/.gpg-agent-info ]
    and kill -0 (cut -d : -f 2 ~/.gpg-agent-info) ^/dev/null
end
    # no, it is not running. Start it!
    gpg-agent --daemon --no-grab --write-env-file ~/.gpg-agent-info >/dev/null ^&1
end
# get the agent info from the info file, and export it so GPG can see it.
set -gx GPG_AGENT_INFO (cut -c 16- ~/.gpg-agent-info)
set -gx GPG_TTY (tty)
```

If you add this to your `config.fish` (or add the bash version to your `.bash_profile`), then all your terminals will have access to a gpg agent. However, GPG won't automatically connect to it. When invoking GPG directly, you can pass `--use-agent`, but git doesn't expose that option. Instead, add the line `use-agent` to your [`~/.gnupg/gpg.conf`](https://github.com/ErinCall/Dotfiles/blob/master/.gnupg/gpg.conf#L199). Now GPG will look for an agent whenever it's invoked.

If you want to adjust how long the agent holds onto your password, adjust the `default-cache-ttl` setting in [`~/.gnupg/gpg-agent.conf`](https://github.com/ErinCall/Dotfiles/blob/master/.gnupg/gpg-agent.conf). The setting is expressed in seconds, so if you want to type your password once per hour, set it to 3600.

There's one other thing that'll annoy you. Behind the scenes, `git stash` creates a commit. That means that you'll have to unlock your GPG key in order to stash. I've created a `stache` alias that stashes without signing: `git config --global alias.stache='!git -c commit.gpgsign=false stash'`. We'll see how reliably I remember to use it.

GPG signing adds an extra layer of insurance, particularly in this age of automated builds and package distribution. Although just showing a "verified" box is a small feature, I expect it'll help push adoption. As signed commits become more common, I'm excited to see how various sites and tools start using them. Maybe Github will even allow [locking down your contribution graph](https://twitter.com/bunsen/status/694527077266104321)...
