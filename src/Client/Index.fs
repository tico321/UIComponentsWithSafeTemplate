module Index

open Elmish
open Fable.Remoting.Client
open Shared

type Page =
    | Home
    | Components
    | Layout

type Model = { CurrentPage: Page }

type Msg =
    | SetPage of Page

let todosApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<ITodosApi>

let init () : Model * Cmd<Msg> =
    let model = { CurrentPage = Home }
    model, Cmd.none

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | SetPage page -> { model with CurrentPage = page }, Cmd.none

open Feliz
open Feliz.Bulma

let homePage dispatch =
    Html.div [
        prop.children [
            Bulma.hero [
                hero.isFullHeight
                color.isPrimary
                prop.style [
                    style.backgroundSize "cover"
                    style.backgroundImageUrl "https://unsplash.it/1200/900?random"
                    style.backgroundPosition "no-repeat center center fixed"
                ]
                prop.children [
                    Bulma.heroBody [
                        Bulma.container [
                            Bulma.column [
                                column.is6
                                column.isOffset3
                                prop.children [
                                    Bulma.title [
                                        text.hasTextCentered
                                        prop.text "Welcome!"
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]

let card () =
    Bulma.card [
        Bulma.cardImage [
            Bulma.image [
                Bulma.image.is4by3
                prop.children [
                    Html.img [
                        prop.alt "Placeholder image"
                        prop.src "https://bulma.io/images/placeholders/1280x960.png"
                    ]
                ]
            ]
        ]
        Bulma.cardContent [
            Bulma.media [
                Bulma.mediaLeft [
                    Bulma.cardImage [
                        Bulma.image [
                            Bulma.image.is48x48
                            prop.children [
                                Html.img [
                                    prop.alt "Placeholder image"
                                    prop.src "https://bulma.io/images/placeholders/96x96.png"
                                ]
                            ]
                        ]
                    ]
                ]
                Bulma.mediaContent [
                    Bulma.title.p [
                        Bulma.title.is4
                        prop.text "Feliz Bulma"
                    ]
                    Bulma.subtitle.p [
                        Bulma.title.is6
                        prop.text "@feliz.bulma"
                    ]
                ]
            ]
            Bulma.content "Lorem ipsum dolor sit ... nec iaculis mauris."
        ]
    ]

let componentsPage dispatch =
    Html.div [
        prop.style [
            style.display.flex
            style.justifyContent.spaceAround
            style.alignItems.center
            style.alignContent.stretch
            style.flexWrap.wrap
            style.rowGap 30
            style.paddingTop 10
        ]
        prop.children [
            Bulma.button.button [ prop.text "Default btn" ]
            Bulma.button.button [ Bulma.color.isLight; prop.text "Light btn" ]
            Bulma.button.button [ Bulma.color.isDark; prop.text "Dark btn"]
            Bulma.button.button [ Bulma.color.isPrimary; prop.text "Primary btn"]

            Bulma.navbar [
                Bulma.color.isPrimary
                prop.children [
                    Bulma.navbarBrand.div [
                        Bulma.navbarItem.a [
                            Html.img [ prop.src "https://bulma.io/images/bulma-logo-white.png"; prop.height 28; prop.width 112; ]
                        ]
                    ]
                    Bulma.navbarMenu [
                        Bulma.navbarStart.div [
                            Bulma.navbarItem.a [ prop.text "Home" ]
                            Bulma.navbarItem.a [ prop.text "Documentation" ]
                            Bulma.navbarItem.a [ prop.text "Jobs" ]
                            Bulma.navbarItem.a [ prop.text "Contact" ]
                            Bulma.navbarItem.a [ prop.text "About" ]
                        ]
                    ]
                ]
            ]

            card ()

            Bulma.progress [ Bulma.color.isPrimary; prop.value 50; prop.max 100; ]

            Bulma.tag [Bulma.color.isPrimary; prop.text "Tag basic"]
            Bulma.tag [Bulma.color.isPrimary;Bulma.color.isLight;prop.text "Tag light"]
            Bulma.tags [
                Bulma.tags.hasAddons
                prop.children [Bulma.tag [Bulma.color.isPrimary;prop.text "Feliz.Bulma"];Bulma.tag [ Bulma.tag.isDelete ]]
            ]
        ]
    ]

let layoutPage dispatch =
    Html.div [
        prop.style [
            style.display.grid
            style.custom ("grid-template-columns", "1fr 1fr")
            style.custom ("grid-template-rows", "1fr 1fr")
            style.height (length.percent 100)
        ]
        prop.children [
            Html.div [
                prop.style [
                    style.backgroundColor "#aaa"
                    style.display.flex
                    style.flexDirection.row
                    style.alignItems.flexEnd
                ]
                prop.text "bottom of the container"
            ]
            Html.div [
                prop.style [
                    style.backgroundColor "#bbb"
                    style.display.flex
                    style.justifyContent.center
                    style.alignItems.center
                ]
                prop.text "centered text"
            ]
            Html.div [
                prop.style [
                    style.backgroundColor "#ccc"
                    style.display.flex
                    style.justifyContent.flexEnd
                ]
                prop.text "to the right"
            ]
            Html.div [
                prop.style [
                    style.backgroundColor "#ddd"
                    style.display.flex
                    style.alignItems.center
                ]
                prop.text "center left"
            ]
        ]
    ]

let menuItem dispatch (text: string) isActive navigateTo =
    Bulma.panelBlock.div [
        prop.classes ["master-item"; (if isActive then "is-active" else "")]
        prop.onClick (fun _ -> dispatch (SetPage navigateTo))
        prop.children [
            Bulma.panelIcon [
                Html.i [ prop.className "fas fa-book" ]
            ]
            Html.span text
        ]
    ]

let masterDetailLayout (model: Model) (dispatch: Msg -> unit) =
    let isActive page =
        match page, model.CurrentPage with
        | Home, Home -> true
        | Components, Components -> true
        | Layout, Layout -> true
        | _ -> false

    Html.div [
        prop.classes ["master-detail-container"]
        prop.children [
            Html.div [
                prop.classes ["master"]
                prop.children [
                    Bulma.panel [
                        Bulma.panelHeading [ prop.text "Menu" ]
                        menuItem dispatch "Home" (isActive Home) Home
                        menuItem dispatch "Components" (isActive Components) Components
                        menuItem dispatch "Layout" (isActive Layout) Layout
                        Bulma.panelBlock.div [
                            prop.classes ["master-lastItem"]
                        ]
                    ]
                ]
            ]

            Html.div [
                prop.classes ["detail"]
                prop.children [
                    match model.CurrentPage with
                    | Home -> homePage dispatch
                    | Components -> componentsPage dispatch
                    | Layout -> layoutPage dispatch
                ]
            ]
        ]
    ]

let view (model: Model) (dispatch: Msg -> unit) =
    Html.div [
        prop.classes ["root"]
        prop.children [
            Bulma.hero [
                color.isPrimary
                prop.children [
                    Bulma.heroBody [
                        Html.p [
                            prop.className ["title"]
                            prop.text "UI Components"
                        ]
                        Html.p [
                            prop.className ["subtitle"]
                            prop.text "Based in the Safe template"
                        ]
                    ]
                ]
            ]

            masterDetailLayout model dispatch
        ]
    ]