<apply template="base">
  <bind tag="page-title">
    Posts tagged <tagName/> | Professional Software Deleter
  </bind>
  <h2>Posts tagged <tagName/></h2>
  <posts>
    <apply template="_partial_post"/>
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
