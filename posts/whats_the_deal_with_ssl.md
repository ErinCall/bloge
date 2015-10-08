Title: What's The Deal With SSL?
Posted: 2015-10-07T18:30:00-0700
Tags:
    ssl
    security

SSL is an important part of the infrastucture of the Internet. It provides three assurances: The computer you're talking to is the one you meant to talk to, the message you're reading was really sent by the computer you're talking to, and the message you're reading hasn't been read by anyone else. I've [mentioned before](/p/setting-up-sni-on-cloudfront) that I think [increasing the cryptographic noise floor](https://www.tbray.org/ongoing/When/201x/2012/12/02/HTTPS) is important, so I thought I'd write a bit about what SSL does and how to put together a strong webserver configuration.

--------------------------8<-----------------------

### What SSL Does

Public-key cryptography is good at ensuring that the message a client received is the one that was sent, and that nobody else has read it. In short, a client exchanges public keys with the server, some math happens, and now the communications are secure.

Verifying a remote server is tricky, though. When you exchange keys, how do you know the server you're talking to is really the server representing someweb.site? In PGP, this problem is addressed with a concept called "[web of trust](https://en.wikipedia.org/wiki/Web_of_trust)," where people you know sign your public key to verify your identity to a third party. It isn't really practical for the Web, though: nobody wants to seek out a friend-of-a-friend-of-a-friend who works at esoteric-craft-supplies-online.com before they can buy some esoteric craft supplies online.

Instead, SSL uses Certificate Authorities (CAs). Operating systems and browsers come with a few dozen CA's public keys preinstalled. When the server begins a handshake, it sends an [X.509 certificate](https://en.wikipedia.org/wiki/X.509#Certificates) that includes (among other things) its public key, the domain(s) for which it's valid, and a cryptographic signature from a certificate authority.

Under most circumstances the client doesn't need to send much of anything beyond a public key. Webservers are happy to talk to anybody, so they don't need to verify anything about a client before commencing communications.

### Concepts In SSL

"SSL" is something of a loaded term. It can refer specifically to the SSL protocol, but [that protocol is deprecated in favor of TLS](https://blogs.akamai.com/2014/10/ssl-is-dead-long-live-tls.html). However, since TLS gained usage gradually, and performs the same role as SSL, "SSL" is used colloquially to refer generally to SSL-like key exchange and encrypted communication. In the rest of this post I'll use "SSL" and "TLS" to refer to the specific protocols, and "SSL/TLS" for the general concept/process.

SSL/TLS uses X.509 certificates, as mentioned above. They're usually stored in `.pem` or `.crt` files, which encode the X.509 data in base 64 with 64-character lines. Certificates use asymmetric (or "public/private-key") cryptographic signatures to provide verification from a trusted third party.

Diffie-Hellman is a mechanism that uses asymmetric cryptography to establish a secure channel on top of an insecure one. It uses large prime numbers and advanced math to devise and exchange a shared secret, after which the shared secret is used in a symmetric cipher like AES.

A reverse proxy webserver is a program that receives HTTP requests from clients and passes them along to application webservers. SSL/TLS is typically handled by a reverse proxy webserver rather than by a specific application. Among other advantages, this practice moves the tricky and security-critical logic into well-understood open-source code, which is more likely to get it right. Nginx and Apache are the most popular reverse-proxy webservers.

A reseller is a company that provides some sort of middleman service between consumers and CAs.

### How You Can Use SSL/TLS

If you want to provide SSL/TLS on your website, you'll need to generate a private key, acquire a certificate, and configure your webserver. In the examples below I'll be using OpenSSL and Nginx. OpenSSL's interface is arcane, and it's acquired a somewhat negative reputation since Heartbleed, but there isn't really anything better available. Nginx is my webserver of choice in most circumstances; Apache users should have little trouble finding translations of the Nginx config provided here.

#### Generate A Private Key

The first part is also the easiest. It's a single OpenSSL command:

```
openssl genrsa -out mywebsite.key 2048
```

The `2048` is the number of bits OpenSSL should use for the key. 2048 is generally considered appropriate.

Remember that `mywebsite.key` is the secret, private key for your website. Don't leave it lying around, and don't put it in source control ([at least not in plaintext](/p/using-pgp-to-encrypt-the-ansible-vault)).

#### Acquire A Certificate

The next step is to decide whether you're buying a single domain name, a wildcard, or some number of sub/domains. What choice you make here depends on what you're trying to do, so I don't have much guidance for you. Keep in mind that a wildcard is only valid for one layer of subdomains: a certificate for `*.yourdomain.com` is **not valid** for `two-level.subdomain.yourdomain.com`.

Pick a CA or reseller (I use [Namecheap](https://www.namecheap.com/security/ssl-certificates.aspx)) and browse their options. Now you can generate a CSR with OpenSSL:

```
openssl req -new -key mywebsite.key -out mywebsite.csr
```

OpenSSL will ask a bunch of questions. The critical one is the 6th or so: `Common Name (e.g. server FQDN or YOUR name)`. If this is to be a wildcard domain, enter `*.yourdomain.com`; otherwise enter `yourdomain.com` or `somesubdomain.yourdomain.com`. If you're the type of person who uses a whois anonymizer, keep in mind that the information in your CSR will end up publically viewable in your certificate.

Send `mywebsite.csr` to your CA/reseller. Some resellers are still signing certificates with the deprecated SHA-1 algorithm, so make sure your CA/reseller provides SHA-256 signatures. Chrome has started displaying user-facing warnings when it encounters certificates signed with SHA-1.

They'll perform some sort of verification and eventually email you a certificate file along with several intermediate certificates. While you wait for the certificates, let's talk about configuring your webserver.

#### Configure Your Webserver

My Nginx config looks somewhat like the below (this all goes in a `server {}` block):

``` nginx
ssl on;
ssl_protocols TLSv1 TLSv1.2;

ssl_ciphers "ECDHE-RSA-AES128-GCM-SHA256:ECDHE-ECDSA-AES128-GCM-SHA256:ECDHE-RSA-AES256-GCM-SHA384:ECDHE-ECDSA-AES256-GCM-SHA384:DHE-RSA-AES128-GCM-SHA256:DHE-DSS-AES128-GCM-SHA256:kEDH+AESGCM:ECDHE-RSA-AES128-SHA256:ECDHE-ECDSA-AES128-SHA256:ECDHE-RSA-AES128-SHA:ECDHE-ECDSA-AES128-SHA:ECDHE-RSA-AES256-SHA384:ECDHE-ECDSA-AES256-SHA384:ECDHE-RSA-AES256-SHA:ECDHE-ECDSA-AES256-SHA:DHE-RSA-AES128-SHA256:DHE-RSA-AES128-SHA:DHE-DSS-AES128-SHA256:DHE-RSA-AES256-SHA256:DHE-DSS-AES256-SHA:DHE-RSA-AES256-SHA:AES128-GCM-SHA256:AES256-GCM-SHA384:AES128-SHA256:AES256-SHA256:AES128-SHA:AES256-SHA:AES:CAMELLIA:DES-CBC3-SHA:!aNULL:!eNULL:!EXPORT:!DES:!RC4:!MD5:!PSK:!aECDH:!EDH-DSS-DES-CBC3-SHA:!EDH-RSA-DES-CBC3-SHA:!KRB5-DES-CBC3-SHA";
ssl_prefer_server_ciphers on;

ssl_dhparam /etc/ssl/dhparams.pem;

ssl_session_cache shared:SSL:50m;
ssl_session_timeout 5m;

ssl_certificate_key /etc/ssl/www_erincall_com.key;
ssl_certificate /etc/ssl/www_erincall_com.chained.crt;
```

The first two lines turn on SSL/TLS and restrict the actual protocol to TLS versions 1 and 1.2. That's a fairly tight restriction, but it works for most browsers. The excluded browsers are--quelle surprise--older versions of IE, and newer versions of IE on older versions of Windows. If you need to support those browsers you'll need to include SSL, although I think you can use versions that have only theoretical vulnerabilities, not practical exploits.

The next two lines, `ssl_ciphers`, and `ssl_prefer_server_ciphers`, control the actual encryption algorithm the TLS protocol will use. You may have heard about the [FREAK](https://en.wikipedia.org/wiki/FREAK) and [Logjam](https://en.wikipedia.org/wiki/Logjam_%28computer_security%29) vulnerabilities, which downgraded SSL/TLS connections to use ["export-grade" cryptography](https://en.wikipedia.org/wiki/Crypto_Wars). These two lines are where you secure your server against those exploits. The list of ciphers is drawn from [weakdh.org](https://weakdh.org/sysadmin.html).

The next line, `ssl_dhparam`, won't work out of the box. Use OpenSSL to create a Diffie-Hellman group:

```
openssl dhparam -out dhparams.pem 2048
```

The `dhparams.pem` file you generate in this way doesn't need to be kept secret, and in fact you _could_ skip this step entirely and use the Diffie-Hellman group that came with your OS. However, recent research has shown that if everyone on the Internet is using the same handful of Diffie-Hellman groups, sophisticated attackers (NSA, GCHQ) can exploit the situation to read targeted encrypted communications. Since it's cheap to generate your own group, and a diverse population of Diffie-Hellman groups makes the attack impractical, you should go ahead and do it.

The next two lines, `ssl_session_cache` and `ssl_session_timeout`, are reasonable defaults for Nginx. You can tune or tweak them if you want, but you probably won't need to.

The final two lines use the private key you generated at the beginning and the certificate from your CA/reseller. By now they've probably finished verification and emailed your certificate to you, so let's get back to that.

#### Put Your Certificate Together

Your CA/reseller has emailed you a certificate, and probably included one or more _intermediate certificates_. Earlier I claimed that your site's cert would be signed by a root CA, but that isn't exactly true. In order to limit the root certificate's private key's exposure, CAs actually sign an intermediate cert with the root cert, then sign site certs with the intermediate cert. You'll need to include the intermediate certs along with your site cert. The "chained" cert should have your site cert at the top, then the intermediate cert that signed your site cert, then the cert that signed the first intermediate...and so on until the root certifate, which you can exclude. For example, I created the chained certificate for blog.erincall.com with this command:

``` Bash
cat blog_erincall_com.crt COMODORSADomainValidationSecureServerCA.crt COMODORSAAddTrustCA.crt > blog_erincall_com.chained.crt
```

The filenames you get from your CA/reseller may be different, of course.

### Self-Signed Certificates

If you don't want to pay for a verified certificate, or you don't want to go to the trouble, you could use a self-signed certificate. Self-signed certificates are inappropriate for production use, but they can be just fine for testing configuration, or for applications like an IRC bouncer, where you're the only user and can just install the certificate locally.

Most of the process is the same, but you get to skip the whole "interacting with a CA" step. After creating your CSR, you play the role of CA:

```
openssl x509 -req -in mywebsite.csr -signkey mywebsite.key -out mywebsite.crt
```

### The Future

With any luck, a lot of the information presented here will be out of date very soon. The [Let's Encrypt](https://letsencrypt.org/) project aims to provide a free, API-accessible CA to support automated SSL/TLS provisioning. In the meantime, keep an eye on your configuration. SSL Labs provides [a free online configuration tester](https://www.ssllabs.com/ssltest/) that will tell you about major or minor problems with your site's configuration. Keep an eye out for SSL/TLS vulnerabilities; if you see one, check back at SSL Labs to see if you're vulnerable. They've been very quick to update their tester.

Stay safe out there!
