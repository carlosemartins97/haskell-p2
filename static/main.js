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

      if (id === "Cadastrar") {
        setTimeout(() => {
          id === "Cadastrar" &&
            document
              .querySelector(".input-group input")
              .setAttribute("placeholder", "Ex: exame de sangue");

          document
            .querySelector("button#cadastra-exame")
            .addEventListener("click", function () {
              const sucessoBtn = document.querySelector(`.conteudo #sucesso`);
              simulateClick(sucessoBtn);

              setTimeout(() => {
                document
                  .querySelector("button#back")
                  .addEventListener("click", function () {
                    const examesBtn =
                      document.querySelector(`.conteudo #Exames`);
                    simulateClick(examesBtn);
                  });
              }, 100);
            });
        }, 100);
      }
    });
  });
});
