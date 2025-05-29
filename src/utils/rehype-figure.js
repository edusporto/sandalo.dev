// Adds caption to images and sets `className` to `img`s.
// Based on <https://github.com/futuraprime/rehype-figure-title>

import { visit } from "unist-util-visit";
import { h } from "hastscript";

export default function rehypeFigure(option) {
  const className = (option && option.className) || "rehype-figure";

  function buildFigure({ properties }) {
    const figure = h("figure", {}, [
      h("img", { ...properties, class: className, title: null }),
      properties.title && properties.title.trim().length > 0
        ? h("figcaption", properties.title)
        : "",
    ]);
    return figure;
  }

  return function (tree) {
    visit(tree, { tagName: "p" }, (node, index) => {
      const images = node.children
        .filter((n) => n.tagName === "img")
        .map((img) => buildFigure(img));

      if (images.length === 0) return;

      tree.children[index] =
        images.length === 1
          ? images[0]
          : h(
            "div",
            { class: `${className}-container`, },
            images,
          );
    });
  };
}
