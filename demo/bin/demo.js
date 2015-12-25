/*
012345678901234567890123456789012345678901234567890123456789012345678901234567890
.         1         2         3         4         5         6         7         8

 * All sites
 */
$(function() {
    // copyable auto-select-all
    $( ".copyable" ).on( "click", function() {
        if ( typeof this.select === "function" ) {
            this.select();
        }
    });

    /*
     * Change navigation elements for smaller screens
     */
    (function() {

        // Move the global nav to the footer and collapse to a select menu
        var globalLinks = $( "#global-nav .links" ).tinyNav({ header: "Browse..." }),
            nav = globalLinks.next(),
            container = $( "<div class='tinynav-container'></div>" ),
            header = $( "<h3><span>More jQuery Sites</span></h3>" );

        container.append( header, nav ).insertBefore( "ul.footer-icon-links" );

        // Collapse the site navigation to a select menu
        $( "#menu-top" ).tinyNav({ header: "Navigate..." });
    })();

    // Banner ads
    (function() {

        // Default site id
        var siteId = 53829,

            // Sites can contain two properties: all and homepage
            site = ({
                "jquery.com": {
                    homepage: 32018
                }
            })[ $( "head" ).attr( "data-live-domain" ) ];

        if ( site ) {
            if ( location.pathname === "/" && site.homepage ) {
                siteId = site.homepage;
            } else if ( site.all ) {
                siteId = site.all;
            }
        }

        window.ados = {
            run: [function() {
                ados_add_placement( 5449, siteId, "broadcast", 1314 );
                ados_load();
            }]
        };

        $.getScript( "//engine.adzerk.net/ados.js" );
    })();
});
