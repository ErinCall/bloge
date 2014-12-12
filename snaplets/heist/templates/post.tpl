<apply template="base">
  <bind tag="page-title"><postTitle/> | Professional Software Deleter</bind>
  <article>
    <apply template="_full_post"/>
  </article>
  <div id="disqus_thread" data-disqusidentifier="${postDisqusId}"></div>
  <script type="text/javascript">
      var disqus_shortname = 'andrewlorente';
      var disqus_identifier = document.getElementById('disqus_thread').dataset.disqusidentifier;

      (function() {
          var dsq = document.createElement('script'); dsq.type = 'text/javascript'; dsq.async = true;
          dsq.src = '//' + disqus_shortname + '.disqus.com/embed.js';
          (document.getElementsByTagName('head')[0]).appendChild(dsq);
      })();
  </script>
  <noscript>If you want to view the comments (I promise they're carefully moderated to ensure high quality), you'll need to turn on JavaScript.</noscript>
  <a href="http://disqus.com" class="dsq-brlink">comments powered by <span class="logo-disqus">Disqus</span></a>

</apply>
