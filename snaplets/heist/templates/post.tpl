<apply template="base">
  <bind tag="page-title"><postTitle/> | You Can't Spell Engineering Without Erin</bind>
  <apply template="_full_post"/>
  <div id="disqus_thread" data-disqusidentifier="${postDisqusId}"></div>
  <script type="text/javascript">
    var disqus_identifier = "${postDisqusId}"
  </script>
  <script src="//andrewlorente.disqus.com/embed.js" async></script>

  <noscript>If you want to view the comments (I promise they're carefully moderated to ensure high quality), you'll need to turn on JavaScript.</noscript>
  <a href="http://disqus.com" class="dsq-brlink">comments powered by <span class="logo-disqus">Disqus</span></a>

</apply>
