package io.github.iltotore.pureparser.web

import cats.effect.IO
import io.github.iltotore.pureparser.Parser
import scala.scalajs.js.annotation.JSExportTopLevel
import tyrian.*
import tyrian.Html.*
import io.github.iltotore.pureparser.ParseError
import scala.collection.mutable.ListBuffer

@JSExportTopLevel("TyrianApp")
object Main extends TyrianIOApp[Msg, Model]:

  override def router: Location => Msg = _ => Msg.NoOp

  override def init(flags: Map[String, String]): (Model, Cmd[IO, Msg]) =
    (Model.default, Cmd.emit(Msg.SetInput(Example.defaultSample)))

  override def update(model: Model): Msg => (Model, Cmd[IO, Msg]) =
    case Msg.NoOp => (model, Cmd.None)
    case Msg.SetInput(value) => (
        model.copy(
          input = value,
          result = Parser(value)(model.selectedExample.parser)
        ),
        Cmd.None
      )

  def exampleSubMenu(example: Example): Html[Msg] = li(cls := "w-3xs")(
    p(cls := "font-bold")(example.name),
    ul(
      example.samples.updated("Blank", "").map((name, content) =>
        li(button(onClick(Msg.SetInput(content)))(name))
      )
        .toSeq*
    )
  )

  def fieldBox(title: String)(children: Html[Msg]*): Html[Msg] = fieldset(cls := "fieldset w-full h-full min-h-0 flex flex-col overflow-hidden")(
    legend(cls := "legend")(title),
    div(cls := "flex flex-col flex-1 rounded-box border border-input overflow-y-auto")(children*)
  )

  def textZone(attributes: Attr[Msg]*)(content: String): Html[Msg] = textarea(
    ((cls := "textarea textarea-md w-full flex-1 resize-none rounded-box border-none font-mono") +: attributes)*
  )(content)

  def viewTree(tree: Tree): Html[Msg] = tree match
    case Tree.Leaf(name, value) => li(a(s"$name = $value"))
    case Tree.Node(name, children) => li(
        details(open)(
          summary(name),
          ul(children.map(viewTree))
        )
      )

  override def view(model: Model): Html[Msg] = div(cls := "w-screen h-screen flex flex-col")(
    div(cls := "navbar shadow-sm px-10")(
      div(cls := "navbar-start text-2xl font-bold")("PureParser examples"),
      ul(cls := "navbar-end menu menu-horizontal rounded-box")(
        li(cls := "w-3xs border border-gray-300 rounded-lg z-50")(
          details(
            summary(cls := "font-bold")(s"Example: ${model.selectedExample.name}"),
            ul(model.examples.map(exampleSubMenu)*)
          )
        )
      )
    ),
    div(cls := "w-full flex-1 min-h-0 py-15 px-10 gap-10 flex flex-row justify-evenly")(
      fieldBox("Input")(
        textZone(onInput(Msg.SetInput.apply))(model.input)
      ),
      div(cls := "w-full h-full flex flex-col gap-5")(
        fieldBox("Output")(
          ul(cls := "menu w-full h-full")(
            model.result.output.fold(div())(viewTree)
          )
        ),
        fieldBox("Errors")(
          ul(cls := "list w-full h-full")(
            model.result.errors.map(error => li(cls := "list-row")(pprint(error).plainText))*
          )
        )
      )
    )
  )

  override def subscriptions(model: Model): Sub[IO, Msg] = Sub.None
