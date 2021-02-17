module Web.View.Static.About where

import           Web.View.Prelude (View (html), hsx, secretCreatorWidget)

data AboutView = AboutView

instance View AboutView where
  html AboutView =
    [hsx|
        <div class="container">
          <h4 class="mdc-typography--headline4">What is it?</h4>
          <p class="mdc-typography--body1">Don't send a password or other form of sensitive information to someone over IM or email. These methods are not secure as anyone with little knowledge can intercept this information during transmission. Using Fidelio you can encrypt your secrets and generate self-destructing links which can be safely transferred to your recipient.</p>
          <h4 class="mdc-typography--headline4">Security features</h4>
          <p class="mdc-typography--body1">Your secret are encrypted client-side on your browser, before being send to the server. Which means it cannot be sniffed by anyone. Decryption is client-side too.
          Moreover, Fidelio is an opensource project and its code can reviewed by everybody at <a href="https://parry84.github.io/fidelio">https://parry84.github.io/fidelio</a>. Pretty secure.</p>
        </div>
    |]
