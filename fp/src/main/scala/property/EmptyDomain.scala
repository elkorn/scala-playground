package fp.property.domain

import fp.Lazy.Empty

case object EmptyDomain extends FiniteDomain[Nothing](Empty, 0)
