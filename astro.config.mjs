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
    ],
    remarkRehype: {
      footnoteLabel: '\0'
    },
    shikiConfig: {
      themes: {
        light: 'github-light',
        dark: 'github-dark'
      }
    }
  },

  server: {
    port: 8080
  },

  build: {
    assets: 'assets'
  },

  outDir: "build",
  compressHTML: false,
  integrations: [mdx()]
});
