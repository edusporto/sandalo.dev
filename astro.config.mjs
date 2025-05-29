// @ts-check
import { defineConfig } from 'astro/config';
import rehypeKatex from 'rehype-katex';
import remarkMath from 'remark-math';
import rehypeFigure from './src/utils/rehype-figure';
import rehypeOverflowMath from './src/utils/rehype-overflow-math';

import mdx from '@astrojs/mdx';

// https://astro.build/config
export default defineConfig({
  site: "https://sandalo.dev",

  markdown: {
    remarkPlugins: [remarkMath],
    rehypePlugins: [
      rehypeKatex,
      [rehypeFigure, { className: "go-center" }],
      rehypeOverflowMath
    ]
  },

  outDir: "build",
  compressHTML: false,
  integrations: [mdx()]
});
