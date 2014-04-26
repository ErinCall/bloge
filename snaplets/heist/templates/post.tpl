<apply template="base">
  <bind tag="page-title"><postTitle/></bind>
  <article>
    <h3><a href="/p/${postSlug}"><postTitle/></a></h3>
    <section class="post-body">
      <postBody/>
    </section>
    <div class="meta">
      <div>Posted on <postPostedDate/></div>
    </div>
  </article>
</apply>
