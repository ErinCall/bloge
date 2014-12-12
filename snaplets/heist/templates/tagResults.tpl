<apply template="base">
  <bind tag="page-title">
    Posts tagged <tagName/> | Professional Software Deleter
  </bind>
  <h2>Posts tagged <tagName/></h2>
  <posts>
    <article>
      <apply template="_partial_post"/>
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
