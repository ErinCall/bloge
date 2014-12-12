Title: Setting Up SNI On Cloudfront
Tags:
    ssl
Posted: 2014-04-29T00:27:07+0000
I have this app [Catsnap](http://catsnap.andrewlorente.com) that I use to organize my photos (as well as gifs I pick up around the internet). It stores the images on Amazon S3, and has a cloudfront distribution attached for OMGFAST load times. The cloudfront distro has an ugly domain, though--"d5hwde6hzncg6.cloudfront.net". The links don't look like something you should click.

So, this problem has a trivial solution, right? Just [make a CNAME](http://www.petekeen.net/dns-the-good-parts) pointing e.g. cdn.andrewlorente.com to d5whatever.cloudfront.net? Yes, BUT: I'd no longer be able to use SSL/TLS. The SSL/TLS model ties certificates to particular domain names, so the certificate Amazon has for \*.cloudfront.net is invalid for cdn.andrewlorente.com (or any domains other than \*.cloudfront.net). [I think using SSL/TLS is important](https://www.tbray.org/ongoing/When/201x/2012/12/02/HTTPS), so that wasn't acceptable.

Fortunately, the [SNI](http://en.wikipedia.org/wiki/Server_Name_Indication) extension to TLS offers a fix for this, and since March 2014, Cloudfront supports it. Let's get into setting it up!

--------8<------------

### Generate a private key and CSR

Amazon requires you to upload the private key you'll use, so I think you should assign a specific key to your CDN. I have a wildcard certificate I could use for my cdn domain, but distributing private keys to anyone--even an entity as reputable as Amazon--is a bad idea, so let's generate a fresh public/private key pair to use with Amazon. Run these commands (you may have to install openssl):

```
openssl genrsa -out cdn_andrewlorente_com.key 2048
openssl req -new -key cdn_andrewlorente_com.key -out cdn_andrewlorente_com.csr
```

Of course, you should replace `cdn_andrewlorente_com` with your own domain. The second command will prompt you for various information; for the most part it's obvious what you should enter. It's imperative, though, that you enter your CDN domain at the "Common Name" prompt, and leave the "challenge password" blank.

You now have two files, a `.key` and a `.csr` (for Certificate Signing Request). The `.key` file is your private key; you should keep it close to your vest. The .csr is public information and you don't have to be careful with it.

Next step is to buy a certificate. Yes, "buy," yes, this will cost money. It may rankle--it does for me, at least--but the good news is it's at least cheap. I [buy my certificates through Namecheap](https://www.namecheap.com/security/ssl-certificates/domain-validation.aspx); for the purposes of a CDN certificate you just need the cheap $9.00/year one. Once you've gone through the purchasing process (which can take quite a while; be patient), you'll eventually receive an email from Comodo, with a zip file containing your certificate.

### Put your certificate and private key into AWS

While you're waiting for the email, let's get the AWS command-line client set up. If you already have `pip` installed, it's easy to install the AWS CLI: `pip install awscli`. You'll also want to configure an access key id and secret for the CLI to use: `aws configure`.

Ok, check your email. Got that zip file? Unzip it to find your certificate, as well as several other certificates representing a "root chain." You'll need to combine the root chain certificates into one large "chain" file. Comodo doesn't provide much information about which order they need to go in. Personally, I had three files: `COMODORSADomainValidationSecureServerCA.crt`, `COMODORSAAddTrustCA.crt`, and `AddTrustExternalCARoot.crt`. You may receive a different set. Whatever you have, you need to concatenate them into a single flie, with the CA Root _last_. Do not include your site's certificate (this may be different from what you're used to when feeding a chain to, say nginx). The command I used was:

```
cat COMODORSADomainValidationSecureServerCA.crt COMODORSAAddTrustCA.crt AddTrustExternalCARoot.crt  >| cdn_andrewlorente_com.chain
```

Now you need to upload to Cloudfront using the AWS CLI. Pick a name for your certificate. It just needs to be meaningful to you; you'll use it in the AWS console. The upload command is:

```
aws iam upload-server-certificate --server-certificate-name $NAME --certificate-body file://cdn_andrewlorente_com.crt  --private-key file://cdn_andrewlorente_com.key --certificate-chain file://cdn_andrewlorente_com.chain --path /cloudfront/production/
```

That command might fail with a MalformedCertificate error:

```
A client error (MalformedCertificate) occurred when calling the UploadServerCertificate operation: Unable to validate certificate chain. The certificate chain must start with the immediate signing certificate, followed by any intermediaries in order.
```

This error means you didn't put the root chain certificates in the right order. Remember that the CA Root certificate goes last, and beyond that, as far as I can see you sorta have to guess and permute.

### Set up your Cloudfront distro to use your certificate

Ok, this has been a lot, but we're almost done. Head to [the AWS console](https://console.aws.amazon.com), sign in, select Cloudfront, select the Cloudfront distro you're using, click Distribution Settings, then click Edit. Add your custom domain to the "Alternate Domain Names (CNAMEs)" field. Select Custom SSL Certificate and grab your certificate by name in the dropdown. Finally, under Custom SSL Support, select "Only Clients that Support Server Name Indication (SNI)". You can choose All Clients instead if you want, but beware! It's $600.00/month. Hit "Yes, Edit" and you're all done in the AWS console.

Keep in mind that it'll take some time--15 minutes or so--for the new settings to propagate out to the various Cloudfront servers. Be patient!

### Create a CNAME

The last step is just to create a CNAME in your DNS provider. The specifics of this are highly tied to your DNS provider, so I won't get into too much detail, but if you go into your DNS provider's console and click CNAME they should make it evident what you need to do next.

### Final thoughts

Amazon does provide [some documentation on this process](http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/SecureConnections.html#CNAMEsAndHTTPS). I found that it was difficult to follow: the simple-path instructions are intermingled with all sorts of info about edge cases and side notes. It's important information to have, but being new to the process, I would've preferred a more focused guide (such as what I've tried to present here).

SNI is not supported on all clients. However, its support is broad enough for most purposes: Everything but WinXP and the default browser on very old versions of Android will work. Anyone using those needs to upgrade before they try to view your fancy html5-enabled site anyway ;~)
