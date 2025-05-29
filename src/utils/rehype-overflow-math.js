// Wraps non-inline math blocks in KaTeX with overflowable divs.

import { visit } from "unist-util-visit";
import { h } from "hastscript";

export default function rehypeKatexOverflowInParagraphs() {
  return function (tree) {
    visit(tree, { tagName: "p" }, (paragraphNode, index, parent) => {
      // We only add overflows to non-inline math blocks
      if (paragraphNode.children.length !== 1) {
        return;
      }

      // Check if the paragraph's single child is `katex`
      const child = paragraphNode.children[0];
      if (child.properties?.className?.includes("katex")) {
        // We replace the paragraph node with an overflowable div with
        // the original paragraph inside
        const overflowDiv = h(
          "div",
          { style: "overflow-x:auto;overflow-y:hidden;" },
          [ h("p", { style: "margin:0" }, [child]) ]
        );

        parent.children[index] = overflowDiv;
      }
    });
  };
}
