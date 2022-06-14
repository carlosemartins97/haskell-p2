document.addEventListener("DOMContentLoaded", function () {
  const menu = document.querySelectorAll("nav li a");

  var simulateClick = function (elem) {
    // Create our event (with options)
    var evt = new MouseEvent("click", {
      bubbles: true,
      cancelable: true,
      view: window,
    });
    // If cancelled, don't dispatch our event
    var canceled = !elem.dispatchEvent(evt);
  };

  menu.forEach((item) => {
    item.addEventListener("click", function () {
      const id = item.textContent;

      const element = document.querySelector(`.conteudo #${id}`);
      simulateClick(element);
    });
  });
});
