---
title: danso.ca and this blog
tags: hacking, haskell, meta
date: 12018-04-14
summary: I introduce my new website to the world.
---

### Welcome to my blog and website

I've been an avid Web user for at least a decade, but in all that time, never made a website of my own. Having now been asked multiple times why not, I present the new [danso.ca](https://danso.ca).

I intend to keep this website updated with all my current events and projects. For those who might want to follow my work, I'll be advertising upcoming concerts and competitions along with program notes, media, and whatnot.

While the main purpose of the website is to showcase my work in music and technology, I also intend to start writing more often. I shared a blog several years ago with a group of friends, but this will be entirely my own thoughts and words. 

I hope to regularly add new posts focusing on the intersection of math and music. I'll also post about ongoing or completed projects and possibly write tech guides. Knowing myself, I probably won't be able to resist the occasional post about ethics or politics as well.

The source code to this website is publicly viewable on [GitLab](https://gitlab.com/danso/danso.ca) and the text is available under a [Creative Commons](http://creativecommons.org/) license.

### Construction

In the spirit of [free culture](https://en.wikipedia.org/wiki/Free-culture_movement), my first blog post will cover how I made this.

This website is built with [Hakyll](https://jaspervdj.be/hakyll/). 

I had only a few goals with my first website: I knew that I wanted a static HTML website that worked entirely without JavaScript. Frankly, I think in the world of [Spectre](https://en.wikipedia.org/wiki/Spectre_(security_vulnerability)), nobody should be browsing the web with JS enabled.[^1] I wanted most of the pages to look mostly the same, but of course I didn't want to duplicate effort. A static site generator made perfect sense for my use-case, and I chose Hakyll without much further research.

[^1]: See [Let’s Replace JavaScript with Something Better](https://john.ankarstrom.se/english/texts/replacing-javascript/). 

Most of the pages are Markdown files, along with the header/footer templates and some CSS. I have basically no idea what I'm doing when it comes to CSS, which is why my website might appear a bit generic. I expect to change it more, over time.

The colour scheme is a subset of [Solarized](http://ethanschoonover.com/solarized), because I'm unoriginal and always use the same colours for everything.

I don't use cabal, because frankly I don't understand it. I tried to once, but things just didn't work out between us. I use a plain old makefile to automate my builds.

### Credit

While I was learning to use Hakyll, I benefitted greatly from Javran Cheng's [tutorial](https://javran.github.io/posts/2014-03-01-add-tags-to-your-hakyll-blog.html) on tags. 

I also owe thanks to Rohan Jain's [post](https://www.rohanjain.in/hakyll-clean-urls/) about generating clean URLs using subdirectories. 

I use both of these features on my website, and more importantly, reading these quickly improved my understanding of the Hakyll system.
