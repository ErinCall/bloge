<apply template="base">
  <bind tag="page-title">Professional Software Deleter</bind>
  <posts>
    <article>
      <apply template="_post"/>
      <a href="/p/${postSlug}#disqus_thread" data-disqus-identifier="${postDisqusId}"></a>
    </article>
  </posts>
  <script type="text/javascript">
    var disqus_shortname = 'andrewlorente';

    (function () {
      var s = document.createElement('script');
      s.async = true;
      s.type = 'text/javascript';
      s.src = '//' + disqus_shortname + '.disqus.com/count.js';
      document.getElementsByTagName('HEAD')[0].appendChild(s);
    }());
  </script>
</apply>
