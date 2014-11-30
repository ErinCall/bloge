Title: Using PGP To Encrypt The Ansible Vault
Slug: using-pgp-to-encrypt-the-ansible-vault
Posted: 2014-11-30T13:00:00-0800
Tags:
    ansible
    pgp
Over the last week I've been getting rid of the extremely janky Puppet setup I had provisioning my VPS, and replacing it with Ansible. One of the features I really like in Ansible is the Vault, which is a fancy name for AES-encrypted data files. The Vault lets me put stuff like API keys in source control without exposing them to my enemies [1]. Super convenient!

Unfortunately, the Vault is also sort of a pain: every time I want to edit an encrypted file, or do a test run, I have to type my Super Complex Secret Passphrase. I had to do a lot of test runs while getting everything verified, so that got pretty tedious. Additionally, if this Ansible setup were for a project with many developers, I'd have [the usual password-distribution problems](https://en.wikipedia.org/wiki/Key_distribution).

Fortunately, there's another way. Ansible has support for getting the Vault passphrase from a script [2], so if I had some authorized agent that could report the passphrase, I'd be golden. [John Knight has a nice writeup on using GPG](https://btl.gs/2014/09/01/using-ansible-vault-and-gpg-to-secure-critical-infrastructure-in-public-github-repositories/), but I'm a little uncomfortable with his approach. He stores a PGP-encrypted Vault passphrase in source control, which is sensible, but then decrypts it to a `.gitignore`d plaintext file. That isn't the kind of thing I want to leave lying around. I use full-disk encryption and all, but I'm still nervous, so I've developed a slightly different approach.

First, I generate a super secret Vault passphrase:

```Bash
pwgen -C | head -n1 | gpg -e -o vault_passphrase.gpg
```

I'm using `pwgen` here, but of course you could read from `/dev/urandom` or whatever strikes your fancy. The point is just to make something nice and unguessable, since humans won't ever have to type it.

Now I make a script called `open_the_vault.sh`. Remember, it needs to be marked executable or Ansible will think its contents themselves are the passphrase.

```Bash
#!/bin/sh
gpg --batch --use-agent --decrypt vault_passphrase.gpg
```

I tell Ansible about it by adding a line to `ansible.cfg`:

```INI
[defaults]
vault_password_file=open_the_vault.sh
```

The last thing is to make sure `gpg-agent` is installed and running. Otherwise, I've just swapped "type an inconveniently-long Vault passphrase" for "type an inconveniently-long PGP passphrase," and that's not even an improvement. I've put a 10-minute TTL on my GPG agent by adding `default-cache-ttl 600` to `~/.gnupg/gpg-agent.conf`, to minimize the window where someone can use my cached private key.

Once I've done all this, I have a real nice situation: my servers' secrets are in source control, but not exposed; I can grant and revoke access with ease; an authorized person must be present to decrypt any secrets; I don't have to type my passphrase over and over and over. As far as I'm concerned, it's the best of all possible worlds!

[1] I don't actually have any enemies. At least, not as far as I know_!_

[2] Since version 1.7.
